open Lexing_utils
open Logf

module Make (Yard : Yard_state_monad.S) : sig
  val run : tokenList -> valueList
end = struct
  let ( let* ) = Yard.bind
  let ( let+ ) = Yard.eval_with

  type depth_counter =
    (* Plain true means there is an operator expected in this moment *)
    | Plain of bool
    (* Depth of nested brackets *)
    | Bracket of int
    (* First one is depth of nested scopes, second is depth of nested Brackets once outside *)
    | Scope of int * int

  let rec run lst = List.rev @@ Yard.init lst (eval ())

  and eval () =
    let rec iter () =
      logf "[Shunting_yard] called iter\n";
      let* tiktok = Yard.get_tiktok in
      let* t = Yard.peek_token in
      if t = EOF then Yard.return ()
      else
        match t with
        | Number _ when tiktok -> Yard.return ()
        | Number n ->
            let* _ = Yard.read_token in
            let* () = Yard.push_value (Number n) in
            iter ()
        | Operator OpenBracket when tiktok -> Yard.return ()
        | Operator OpenBracket ->
            let* _ = Yard.read_token in
            let* () = Yard.push_op OpenBracket in
            iter ()
        | Operator OpenScope when tiktok -> Yard.return ()
        | Operator OpenScope ->
            let* _ = Yard.read_token in
            let* () = Yard.push_assign ScopeBorder in
            iter ()
        | Operator (Boolean Not) when tiktok -> Yard.return ()
        | Operator (Boolean Not as op) ->
            let* _ = Yard.read_token in
            let* () = try_push_op op in
            iter ()
        | Operator _ when not tiktok ->
            Yard.failwith @@ Errors.reportOperatorCumulation
        | Operator Semi ->
            let* _ = Yard.read_token in
            (* somehow fold stack *)
            handle_semicolon ()
        | Operator CloseBracket ->
            let* _ = Yard.read_token in
            let* () = close_bracket () in
            iter ()
        | Operator CloseScope ->
            let* _ = Yard.read_token in
            let* () = close_scope () in
            iter ()
        | Operator op ->
            let* _ = Yard.read_token in
            let* () = try_push_op op in
            iter ()
        | Bool _ when tiktok -> Yard.return ()
        | Bool b ->
            let* _ = Yard.read_token in
            let* () = Yard.push_value (Bool b) in
            iter ()
        | Unit when tiktok -> Yard.return ()
        | Unit ->
            let* _ = Yard.read_token in
            let* () = Yard.push_value Unit in
            iter ()
        | If when tiktok -> Yard.return ()
        | If ->
            let* _ = Yard.read_token in
            let* cond = eval_one () in
            let* then_branch = prepare () in
            let* else_branch = prepare () in
            let* () =
              match cond with
              | Bool b when b ->
                  let* res = eval_prepared then_branch in
                  Yard.push_value res
              | Bool _ ->
                  let* res = eval_prepared else_branch in
                  Yard.push_value res
              | _ -> Yard.failwith @@ Errors.reportTypeError "Bool" cond
            in
            iter ()
        | Id _ when tiktok -> Yard.return ()
        | Id id ->
            let* _ = Yard.read_token in
            let* () = Yard.push_value (Id id) in
            iter ()
        | Builtin _ when tiktok -> Yard.return ()
        | Builtin b ->
            let* _ = Yard.read_token in
            let* () = eval_builtin b in
            iter ()
        | EOF -> Yard.failwith @@ Errors.reportUnexpectedEOF None
    and wrap_iter () =
      logf "[Shunting_yard] called wrap_iter\n";
      let* () = Yard.push_op StackSeparator in
      let* () = iter () in
      let* () = finish_eval () in
      let* t = Yard.peek_token in
      match t with EOF -> Yard.return () | _ -> wrap_iter ()
    in
    logf "[Shunting_yard] called eval\n";
    wrap_iter ()

  and prepare () =
    logf "[Shunting_yard] called prepare\n";
    let rec prepare_one counter : tokenList Yard.t =
      let* t = Yard.peek_token in
      match (counter, t) with
      | Plain b, Operator Semi when b ->
          let* x = Yard.read_token in
          Yard.return [ x ]
      | Plain _, Operator CloseBracket ->
          Yard.failwith @@ Errors.reportUnexpectedClosing "bracket"
      | Plain _, Operator CloseScope ->
          Yard.failwith @@ Errors.reportUnexpectedClosing "bracket"
      | Plain _, Operator Semi ->
          Yard.failwith @@ Errors.reportUnexpectedToken "Value" t
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
      | Plain _, Operator _ ->
          Yard.failwith @@ Errors.reportUnexpectedToken "Value" t
      | Plain b, EOF when not b ->
          Yard.failwith @@ Errors.reportUnexpectedEOF (Some "Value")
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
          Yard.failwith @@ Errors.reportUnexpectedClosing "scope"
      | Bracket _, EOF ->
          Yard.failwith
          @@ Errors.reportUnexpectedEOF (Some "closing of bracket")
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
      | Scope _, EOF ->
          Yard.failwith @@ Errors.reportUnexpectedEOF (Some "closing of scope")
      | Scope (i, j), _ ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Scope (i, j)) in
          Yard.return (x :: xs)
    in
    prepare_one (Plain false)

  and eval_one () =
    logf "[Shunting_yard] called eval_one\n";
    let* prepared = prepare () in
    eval_prepared prepared

  and eval_prepared prepared =
    logf "[Shunting_yard] called eval_prepared\n";
    let+ () = (prepared, eval) in
    Yard.pop_value

  and eval_builtin builtin =
    logf "[Shunting_yard] called eval_builtin\n";
    match builtin with
    | Fst -> (
        let* v = eval_one () in
        match v with
        | Pair (v, _) -> Yard.push_value v
        | _ -> Yard.failwith @@ Errors.reportTypeError "Pair" v)
    | Snd -> (
        let* v = eval_one () in
        match v with
        | Pair (_, v) -> Yard.push_value v
        | _ -> Yard.failwith @@ Errors.reportTypeError "Pair" v)
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
        | _ -> Yard.failwith @@ Errors.reportTypeError "Number" v)
    | At -> (
        let* v = eval_one () in
        let* value_stack = Yard.value_stack in
        match v with
        | Number n -> Yard.push_value @@ ValueSeq.nth n value_stack
        | _ -> Yard.failwith @@ Errors.reportTypeError "Number" v)
    | NumberPred -> (
        let* v = eval_one () in
        match v with
        | Number _ -> Yard.push_value (Bool true)
        | _ -> Yard.push_value (Bool false))
    | UnitPred -> (
        let* v = eval_one () in
        match v with
        | Unit -> Yard.push_value (Bool true)
        | _ -> Yard.push_value (Bool false))
    | BoolPred -> (
        let* v = eval_one () in
        match v with
        | Bool _ -> Yard.push_value (Bool true)
        | _ -> Yard.push_value (Bool false))
    | PairPred -> (
        let* v = eval_one () in
        match v with
        | Pair _ -> Yard.push_value (Bool true)
        | _ -> Yard.push_value (Bool false))

  and eval_op op =
    logf "[Shunting_yard] called eval_op\n";
    match op with
    | Arthmetic op -> (
        let* rhs = Yard.pop_value in
        let* lhs = Yard.pop_value in
        try
          match (lhs, rhs) with
          | Number lhs, Number rhs -> Yard.push_value @@ eval_binop lhs rhs op
          | _ ->
              Yard.failwith
              @@ Errors.reportOperatorArgsTypeError "Numbers" lhs rhs
        with Division_by_zero -> Yard.failwith @@ Errors.reportDivisionByZero)
    | Relation op -> (
        let* rhs = Yard.pop_value in
        let* lhs = Yard.pop_value in
        match (lhs, rhs) with
        | Number lhs, Number rhs -> Yard.push_value @@ eval_relop lhs rhs op
        | _ ->
            Yard.failwith
            @@ Errors.reportOperatorArgsTypeError "Numbers" lhs rhs)
    | Boolean Not -> (
        let* arg = Yard.pop_value in
        match arg with
        | Bool b -> Yard.push_value (Bool (not b))
        | _ -> Yard.failwith @@ Errors.reportTypeError "Bool" arg)
    | Boolean op -> (
        let* rhs = Yard.pop_value in
        let* lhs = Yard.pop_value in
        match (lhs, rhs, op) with
        | Bool lhs, Bool rhs, And -> Yard.push_value @@ Bool (lhs && rhs)
        | Bool lhs, Bool rhs, Or -> Yard.push_value @@ Bool (lhs || rhs)
        | _ ->
            Yard.failwith @@ Errors.reportOperatorArgsTypeError "Bools" lhs rhs)
    | Comma -> Yard.failwith "Not Implemented"
    | Assign -> Yard.failwith "Not Implemented"
    | Binding -> Yard.failwith "Not Implemented"
    | Apply -> Yard.failwith "Not Implemented"
    | _ -> Yard.failwith "This should be unreachable"

  and close_bracket () =
    logf "[Shunting_yard] called close_bracket\n";
    let* op = Yard.pop_op in
    match op with
    | OpenBracket -> Yard.return ()
    | _ ->
        let* () = eval_op op in
        close_bracket ()

  and try_push_op op =
    logf "[Shunting_yard] called try_push_op\n";
    let* top_op = Yard.peek_op in
    try
      if
        operator_prio top_op > operator_prio op
        || operator_prio top_op = operator_prio op
           && operator_binding op = Left
      then
        let* top_op = Yard.pop_op in
        let* () = eval_op top_op in
        try_push_op op
      else Yard.push_op op
    with Invalid_argument s -> Yard.failwith s

  and finish_eval () =
    logf "[Shunting_yard] called finish_eval\n";
    let* top_op = Yard.pop_op in
    if top_op = StackSeparator then Yard.return ()
    else if top_op = OpenBracket then
      Yard.failwith @@ Errors.reportUnclosedBracketWhileFolding
    else
      let* () = eval_op top_op in
      finish_eval ()

  and close_scope () = Yard.failwith "Not Implemented"
  and handle_semicolon () = Yard.failwith "Not Implemented"
end
