open Lexing_utils

module type S = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  (** pass tokenList and function to execute with it, then recover previous state *)
  val eval_with : tokenList * (unit -> 'a t) -> ('a -> 'b t) -> 'b t

  (** start monad with tokenList *)
  val init : tokenList -> 'a t -> 'a

  (** pop token from list and return it *)
  val read_token : token t

  (** read token from list without modifying it *)
  val peek_token : token t

  (** push value to value stack *)
  val push_value : value -> unit t

  (** push operator to operator stack *)
  val push_op : operator -> unit t

  (** push assignment to assignment stack *)
  val push_assign : assignment -> unit t

  (** pop value from the stack and return it *)
  val pop_value : value t

  (** pop operator from the stack and return it *)
  val pop_op : operator t

  (** read operator from the stack without modifying it *)
  val peek_op : operator t

  (** pop assignment from the stack and return it *)
  val pop_assign : assignment t

  (** view value stack *)
  val value_stack : valueList t

  (** get tiktok state *)
  val get_tiktok : bool t

  (** failwith error *)
  val failwith : string -> 'a t

end

module Make (Yard : S) = struct
  let ( let* ) = Yard.bind
  let ( let+ ) = Yard.eval_with

  type depth_counter =
    (* Plain true means there is an operator expected in this moment *)
    | Plain of bool
    (* Depth of nested brackets *)
    | Bracket of int
    (* First one is depth of nested scopes, second is depth of nested Brackets once outside *)
    | Scope of int * int

  let rec eval () =
    let* tiktok = Yard.get_tiktok in
    let* t = Yard.read_token in
    match t with
    | Number n -> Yard.push_value (Number n)
    | Operator _ when not tiktok ->
        Yard.failwith "Syntax Error: operator cumulation"
    | Operator Semi ->
        (* somehow fold stack *)
        handle_semicolon ()
    | Operator OpenBracket -> Yard.push_op OpenBracket
    | Operator CloseBracket -> close_bracket ()
    | Operator OpenScope -> Yard.push_assign ScopeBorder
    | Operator CloseScope -> close_scope ()
    | Operator op -> try_push_op op
    | Bool b -> Yard.push_value (Bool b)
    | Unit -> Yard.push_value Unit
    | If -> (
        let* cond = eval_one () in
        let* then_branch = prepare () in
        let* else_branch = prepare () in
        match cond with
        | Bool b when b ->
            let* res = eval_prepared then_branch in
            Yard.push_value res
        | Bool _ ->
            let* res = eval_prepared else_branch in
            Yard.push_value res
        | _ -> Yard.failwith "Type Error: condition expected to be of type bool"
        )
    | Id id -> Yard.push_value (Id id)
    | Builtin b -> eval_builtin b
    | EOF -> Yard.failwith "Syntax Error: unexpected end of file"

  and prepare () =
    let rec prepare_one counter : tokenList Yard.t =
      let* x = Yard.peek_token in
      match (counter, x) with
      | Plain b, Operator Semi when b ->
          let* x = Yard.read_token in
          Yard.return [ x ]
      | Plain _, Operator CloseBracket ->
          Yard.failwith "Syntax Error: Unexpected closing of unopened bracket"
      | Plain _, Operator CloseScope ->
          Yard.failwith "Syntax Error: Unexpected closing of unopened scope"
      | Plain _, Operator Semi ->
          Yard.failwith "Syntax Error: Expected Value gotten Operator(Semi)"
      | Plain b, Operator (Boolean Not)
      | Plain b, Builtin _
      | Plain b, Operator OpenBracket
        when b ->
          Yard.return []
      | Plain _, Operator OpenScope ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Scope (1, 0)) in
          Yard.return (x :: xs)
      | Plain _, Operator OpenBracket ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Bracket 1) in
          Yard.return (x :: xs)
      | Plain _, Builtin Readc ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Plain true) in
          Yard.return (x :: xs)
      | Plain _, Operator (Boolean Not) | Plain _, Builtin _ ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Plain false) in
          Yard.return (x :: xs)
      | Plain b, Operator _ when b ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Plain false) in
          Yard.return (x :: xs)
      | Plain _, Operator op ->
          Yard.failwith
          @@ Printf.sprintf "Syntax Error: Expected Value gotten Operator(%s)"
          @@ string_of_operator op
      | Plain b, EOF when not b ->
          Yard.failwith "Syntax Error: Expected Value gotten End_of_file"
      | Plain b, If when not b ->
          let* x = Yard.read_token in
          let* cond = prepare_one (Plain false) in
          let* then_branch = prepare_one (Plain false) in
          let* else_branch = prepare_one (Plain false) in
          Yard.return @@ (x :: List.flatten [ cond; then_branch; else_branch ])
      | Plain b, _ when b -> Yard.return []
      | Plain _, _ ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Plain true) in
          Yard.return (x :: xs)
      | Bracket i, Operator OpenBracket ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Bracket (i + 1)) in
          Yard.return (x :: xs)
      | Bracket i, Operator CloseBracket when i = 1 ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Plain true) in
          Yard.return (x :: xs)
      | Bracket i, Operator CloseBracket ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Bracket (i - 1)) in
          Yard.return (x :: xs)
      | Bracket i, Operator OpenScope ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Scope (1, i)) in
          Yard.return (x :: xs)
      | Bracket _, Operator CloseScope ->
          Yard.failwith "Syntax Error: Unexpected closing of unopened scope"
      | Bracket _, EOF ->
          Yard.failwith
            "Syntax Error: Expected closing of bracket gotten End_of_file"
      | Bracket _, _ ->
          let* x = Yard.read_token in
          let* xs = prepare_one counter in
          Yard.return (x :: xs)
      | Scope (i, j), Operator OpenScope ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Scope (i + 1, j)) in
          Yard.return (x :: xs)
      | Scope (i, j), Operator CloseScope when i = 1 ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Bracket j) in
          Yard.return (x :: xs)
      | Scope (i, j), Operator CloseScope ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Scope (i - 1, j)) in
          Yard.return (x :: xs)
      | Scope (i, j), _ ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Scope (i, j)) in
          Yard.return (x :: xs)
    in
    prepare_one (Plain false)

  and eval_one () =
    let* prepared = prepare () in
    eval_prepared prepared

  and eval_prepared prepared =
    let+ () = (prepared, eval) in
    Yard.pop_value

  and eval_builtin = function
    | Fst -> (
        let* v = eval_one () in
        match v with
        | Pair (v, _) -> Yard.push_value v
        | _ -> Yard.failwith @@ Error.reportTypeError "Pair" v)
    | Snd -> (
        let* v = eval_one () in
        match v with
        | Pair (_, v) -> Yard.push_value v
        | _ ->
            Yard.failwith
            @@ Printf.sprintf "Type Error: Expected Pair, Gotten: %s"
                 (string_of_value v))
    | Readc ->
        (* TODO: Implement reading from stdin *)
        (* For now always returns zero        *)
        Yard.push_value (Number 0)
    | Writec -> (
        let* v = eval_one () in
        match v with
        | Number n ->
            Printf.printf "%c" (char_of_int (n mod 256));
            Yard.return ()
        (* TODO: Maybe implement folding list to act like string *)
        | _ ->
            Yard.failwith
            @@ Printf.sprintf "Type Error: Expected Number gotten %s"
            @@ string_of_value v)
    | At -> (
        let* v = eval_one () in
        match v with
        | Number n -> Yard.push_value @@ ValueSeq.nth n @@ Yard.value_stack
        | _ ->
            Yard.failwith
            @@ Printf.sprintf "Type Error: Expected Number gotten %s"
            @@ string_of_value v)
    | NumberPred -> (
        let* v = eval_one () in
        match v with
        | Number _ -> Yard.push_value Bool true
        | _ -> Yard.push_value Bool false)
    | UnitPred -> (
        let* v = eval_one () in
        match v with
        | Unit -> Yard.push_value Bool true
        | _ -> Yard.push_value Bool false)
    | BoolPred -> (
        let* v = eval_one () in
        match v with
        | Bool _ -> Yard.push_value Bool true
        | _ -> Yard.push_value Bool false)
    | PairPred -> (
        let* v = eval_one () in
        match v with
        | Pair _ -> Yard.push_value Bool true
        | _ -> Yard.push_value Bool false)
    | _ -> failwith "Not Implemented"

  and eval_op = function
    | Arthmetic op -> (
        let* rhs = Yard.pop_value in
        let* lhs = Yard.pop_value in
        match (lhs, rhs) with
        | Number lhs, Number rhs -> Yard.push_value @@ eval_binop lhs rhs op
        | _ ->
            Yard.failwith
            @@ Printf.sprintf "Type Error: Expected Numbers, gotten: (%s, %s)"
                 (string_of_value lhs) (string_of_value rhs))
    | Relation op -> (
        let* rhs = Yard.pop_value in
        let* lhs = Yard.pop_value in
        match (lhs, rhs) with
        | Number lhs, Number rhs -> Yard.push_value @@ eval_relop lhs rhs op
        | _ ->
            Yard.failwith
            @@ Printf.sprintf "Type Error: Expected Numbers, gotten: (%s, %s)"
                 (string_of_value lhs) (string_of_value rhs))
    | Boolean Not -> (
        let* arg = Yard.pop_value in
        match arg with
        | Bool b -> Yard.push_value (Bool (not b))
        | _ ->
            Yard.failwith
            @@ Printf.sprintf "Type Error: Expected Bool, gotten: %s"
            @@ string_of_value arg)
    | Boolean op -> (
        let* rhs = Yard.pop_value in
        let* lhs = Yard.pop_value in
        match (lhs, rhs, op) with
        | Bool lhs, Bool rhs, And -> Yard.push_value @@ Bool (lhs && rhs)
        | Bool lhs, Bool rhs, Or -> Yard.push_value @@ Bool (lhs || rhs)
        | _ ->
            Yard.failwith
            @@ Printf.sprintf "Type Error: Expected Numbers, gotten: (%s, %s)"
                 (string_of_value lhs) (string_of_value rhs))
    | Comma -> failwith "Not Implemented"
    | Assign -> failwith "Not Implemented"
    | Binding -> failwith "Not Implemented"
    | Apply -> failwith "Not Implemented"
    | _ -> failwith "This should be unreachable"

  and close_bracket () = failwith "Not Implemented"

  and try_push_op op =
    let* top_op = Yard.pop_op in
    if
      operator_prio top_op > operator_prio op
      || (operator_prio top_op = operator_prio op && operator_binding op = Left)
    then
      let* () = eval_op top_op in
      try_push_op op
    else Yard.push_op op

  and close_scope () = failwith "Not Implemented"
  and handle_semicolon () = failwith "Not Implemented"
end
