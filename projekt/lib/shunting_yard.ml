open Lexing_utils
open Lexing_types
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

  let string_of_depth = function
    | Plain b -> "Plain(" ^ string_of_bool b ^ ")"
    | Bracket n -> "Bracket(" ^ string_of_int n ^ ")"
    | Scope (i, j) -> "Scope(" ^ string_of_int i ^ ", " ^ string_of_int j ^ ")"

  let prepare () =
    logf "[Shunting_yard] called prepare\n";
    let rec prepare_one counter : tokenList Yard.t =
      let* t = Yard.peek_token in
      match (counter, t) with
      | Plain b, Operator Semi
      | Plain b, Operator CloseBracket
      | Plain b, Operator CloseScope
      | Plain b, Operator (Boolean Not)
      | Plain b, Builtin _
      | Plain b, Operator OpenBracket
        when b ->
          Yard.return []
      | Plain _, Operator CloseBracket
      | Plain _, Operator CloseScope
      | Plain _, Operator Semi ->
          Yard.failwith @@ Errors.reportUnexpectedToken "Value" t
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
      | Plain _, Operator (Boolean Not) ->
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
      | Scope (i, j), Operator CloseScope when i = 1 && j = 0 ->
          let* x = Yard.read_token in
          let* xs = prepare_one (Plain true) in
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
    let* res = prepare_one (Plain false) in
    logf "[Shunting_yard:prepare] preparing finished\n";
    Yard.return res

  let rev_map fn lst =
    let rec mapper acc lst =
      match lst with
      | value :: lst ->
          let* value = fn value in
          mapper (value :: acc) lst
      | [] -> Yard.return acc
    in
    mapper [] lst

  let rec run lst =
    let eval =
      let* () = eval () in
      let* stack = Yard.value_stack in
      rev_map deref_val stack
    in
    Yard.init lst eval

  and eval_iter () =
    logf "[Shunting_yard] called eval_iter\n";
    let* tiktok = Yard.get_tiktok in
    let* t = Yard.peek_token in
    match (t, tiktok) with
    | Number _, Some true -> Yard.return ()
    | Number n, _ ->
        let* _ = Yard.read_token in
        let* () = Yard.push_value (Number n) in
        eval_iter ()
    | Operator OpenBracket, Some true -> Yard.return ()
    | Operator OpenBracket, _ ->
        let* _ = Yard.read_token in
        let* () = Yard.push_op OpenBracket in
        eval_iter ()
    | Operator OpenScope, Some true -> Yard.return ()
    | Operator OpenScope, _ ->
        let* _ = Yard.read_token in
        let* () = Yard.push_value ScopeBorder in
        let* () = Yard.push_assign ScopeBorder in
        eval_iter ()
    | Operator (Boolean Not), Some true -> Yard.return ()
    | Operator (Boolean Not as op), _ ->
        let* _ = Yard.read_token in
        let* () = try_push_op op in
        eval_iter ()
    | Operator Semi, Some true ->
        let* _ = Yard.read_token in
        handle_semicolon ()
    | Operator Semi, Some false ->
        Yard.failwith @@ Errors.reportOperatorCumulation
    | Operator Semi, None ->
        Yard.failwith @@ Errors.reportUnexpectedToken "Value" t
    | Operator Binding, Some true ->
        let* _ = Yard.read_token in
        let* _ = handle_binding () in
        eval_iter ()
    | Operator CloseBracket, Some true ->
        let* _ = Yard.read_token in
        let* () = close_bracket () in
        eval_iter ()
    | Operator CloseScope, None ->
        let* _ = Yard.read_token in
        let* op = Yard.pop_op in
        assert (op = StackSeparator);
        let* () = deref_stack () in
        let* () = close_scope () in
        eval_iter ()
    | Operator CloseScope, Some true ->
        let* _ = Yard.read_token in
        let* () = deref_stack () in
        let* () = close_scope () in
        eval_iter ()
    | Operator op, Some true ->
        let* _ = Yard.read_token in
        let* () = try_push_op op in
        eval_iter ()
    | Bool _, Some true -> Yard.return ()
    | Bool b, _ ->
        let* _ = Yard.read_token in
        let* () = Yard.push_value (Bool b) in
        eval_iter ()
    | Unit, Some true -> Yard.return ()
    | Unit, _ ->
        let* _ = Yard.read_token in
        let* () = Yard.push_value Unit in
        eval_iter ()
    | If, Some true -> Yard.return ()
    | If, _ -> (
        let* _ = Yard.read_token in
        let* cond = eval_one () in
        let* then_branch = prepare () in
        let* else_branch = prepare () in
        match cond with
        | Bool b when b ->
            logf "[Shunting_yard:eval_iter:If] choosen then_branch\n";
            let* () = eval_prepared then_branch in
            eval_iter ()
        | Bool _ ->
            logf "[Shunting_yard:eval_iter:If] choosen else_branch\n";
            let* () = eval_prepared else_branch in
            eval_iter ()
        | _ -> Yard.failwith @@ Errors.reportTypeError "Bool" cond)
    | Id _, Some true -> Yard.return ()
    | Id id, _ ->
        let* _ = Yard.read_token in
        let* () = Yard.push_value (Id id) in
        eval_iter ()
    | Builtin _, Some true -> Yard.return ()
    | Builtin b, _ ->
        let* _ = Yard.read_token in
        let* () =
          match b with
          | Readc ->
              (* TODO: Implement reading from stdin *)
              (* For now always returns zero        *)
              let res = Scanf.scanf "%c" (fun c -> Number (int_of_char c)) in
              logf "[Shunting_yard:eval_builtin] evaling Readc -> %s\n"
              @@ string_of_value res;
              Yard.push_value res
          | _ -> Yard.push_value @@ Builtin b
        in
        (* let* () = Yard.set_tiktok false in *)
        eval_iter ()
    | EOF, Some false -> Yard.failwith @@ Errors.reportUnexpectedEOF None
    | EOF, _ -> Yard.return ()
    | Operator _, _ -> Yard.failwith @@ Errors.reportOperatorCumulation

  and eval_builtin builtin arg =
    logf "[Shunting_yard] called eval_builtin with %s\n"
    @@ string_of_builtin builtin;
    match builtin with
    | Fst -> (
        logf "[Shunting_yard:eval_builtin] evaling Fst with %s\n"
        @@ string_of_value arg;
        match arg with
        | Pair (v, _) -> Yard.push_value v
        | _ -> Yard.failwith @@ Errors.reportTypeError "Pair" arg)
    | Snd -> (
        logf "[Shunting_yard:eval_builtin] evaling Snd with %s\n"
        @@ string_of_value arg;
        match arg with
        | Pair (_, v) -> Yard.push_value v
        | _ -> Yard.failwith @@ Errors.reportTypeError "Pair" arg)
    | Writec -> (
        logf "[Shunting_yard:eval_builtin] evaling Writec with %s"
        @@ string_of_value arg;
        match arg with
        | Number n ->
            let c = char_of_int @@ (n mod 256) in
            logf " -> %c\n" c;
            Printf.printf "%c" c;
            Yard.push_value arg
        (* TODO: Maybe implement folding list to act like string *)
        | _ ->
            logf "\n";
            Yard.failwith @@ Errors.reportTypeError "Number" arg)
    | At -> (
        let rec nth n (lst : value list) =
          match lst with
          | [] -> Unit
          | ScopeBorder :: lst -> nth n lst
          | x :: _ when n = 0 -> x
          | _ :: lst -> nth (n - 1) lst
        in
        logf "[Shunting_yard:eval_builtin] evaling At with %s\n"
        @@ string_of_value arg;
        let* value_stack = Yard.value_stack in
        match arg with
        | Number n -> Yard.push_value @@ nth n value_stack
        | _ -> Yard.failwith @@ Errors.reportTypeError "Number" arg)
    | NumberPred -> (
        logf "[Shunting_yard:eval_builtin] evaling NumberPred with %s"
        @@ string_of_value arg;
        match arg with
        | Number _ ->
            logf " -> true\n";
            Yard.push_value (Bool true)
        | _ ->
            logf " -> false\n";
            Yard.push_value (Bool false))
    | UnitPred -> (
        logf "[Shunting_yard:eval_builtin] evaling UnitPred with %s"
        @@ string_of_value arg;
        match arg with
        | Unit ->
            logf " -> true\n";
            Yard.push_value (Bool true)
        | _ ->
            logf " -> false\n";
            Yard.push_value (Bool false))
    | BoolPred -> (
        logf "[Shunting_yard:eval_builtin] evaling BoolPred with %s"
        @@ string_of_value arg;
        match arg with
        | Bool _ ->
            logf " -> true\n";
            Yard.push_value (Bool true)
        | _ ->
            logf " -> false\n";
            Yard.push_value (Bool false))
    | PairPred -> (
        logf "[Shunting_yard:eval_builtin] evaling PairPred with %s"
        @@ string_of_value arg;
        match arg with
        | Pair _ ->
            logf " -> true\n";
            Yard.push_value (Bool true)
        | _ ->
            logf " -> false\n";
            Yard.push_value (Bool false))
    | Readc ->
        Yard.failwith
          ("This should be unreachable, encountered builtin: "
         ^ string_of_builtin Readc)

  and eval_op op =
    logf "[Shunting_yard] called eval_op with %s\n" @@ string_of_operator op;
    match op with
    | Arthmetic op -> (
        let* rhs = pop_derefed () in
        let* lhs = pop_derefed () in
        try
          match (lhs, rhs) with
          | Number lhs, Number rhs -> Yard.push_value @@ eval_binop lhs rhs op
          | _ ->
              Yard.failwith
              @@ Errors.reportOperatorArgsTypeError "Numbers" lhs rhs
        with Division_by_zero -> Yard.failwith @@ Errors.reportDivisionByZero)
    | Relation op -> (
        let* rhs = pop_derefed () in
        let* lhs = pop_derefed () in
        match (lhs, rhs) with
        | Number lhs, Number rhs -> Yard.push_value @@ eval_relop lhs rhs op
        | _ ->
            Yard.failwith
            @@ Errors.reportOperatorArgsTypeError "Numbers" lhs rhs)
    | Boolean Not -> (
        let* arg = pop_derefed () in
        match arg with
        | Bool b -> Yard.push_value (Bool (not b))
        | _ -> Yard.failwith @@ Errors.reportTypeError "Bool" arg)
    | Boolean op -> (
        let* rhs = pop_derefed () in
        let* lhs = pop_derefed () in
        match (lhs, rhs, op) with
        | Bool lhs, Bool rhs, And -> Yard.push_value @@ Bool (lhs && rhs)
        | Bool lhs, Bool rhs, Or -> Yard.push_value @@ Bool (lhs || rhs)
        | _ ->
            Yard.failwith @@ Errors.reportOperatorArgsTypeError "Bools" lhs rhs)
    | Comma ->
        let* rhs = pop_derefed () in
        let* lhs = pop_derefed () in
        Yard.push_value (Pair (lhs, rhs))
    | Assign -> (
        let* rhs = pop_derefed () in
        let* lhs = Yard.pop_value in
        logf "[Shunting_yard:eval_op:Assign] evaluating %s = %s\n"
          (string_of_value lhs) (string_of_value rhs);
        match (lhs, rhs) with
        | Id var, (Lambda _ as body) ->
            let env = Hashtbl.create 3 in
            let clo = Closure (var, env, body) in
            Hashtbl.add env var clo;
            let* () = Yard.push_assign @@ Assign (var, clo) in
            Yard.push_value clo
        | Id var1, Closure (var2, env, body) ->
            let new_env = Hashtbl.copy env in
            let clo = Closure (var1 ^ "$" ^ var2, new_env, body) in
            Hashtbl.replace new_env var1 clo;
            let* () = Yard.push_assign @@ Assign (var1, clo) in
            Yard.push_value clo
        | Id var, _ ->
            let* () = Yard.push_assign @@ Assign (var, rhs) in
            Yard.push_value rhs
        | _ -> Yard.failwith @@ Errors.reportTypeError "Id" lhs)
    | Apply -> (
        let* rhs = pop_derefed () in
        let* lhs = pop_derefed () in
        logf "[Shunting_yard:eval_op:Apply] evaluating %s $ %s\n"
          (string_of_value ~extended_lambda:false lhs)
          (string_of_value ~extended_lambda:false rhs);
        match lhs with
        | Lambda (var, body) ->
            let env = Hashtbl.create 7 in
            logf
              "[Shunting_yard:eval_op:Apply:Lambda] building clo<%s> with [%s \
               : %s]\n"
              var var (string_of_value rhs);
            Hashtbl.add env var rhs;
            logf "[Shunting_yard:eval_op:Apply:Lambda] env after: [%s]\n"
              (string_of_closure env (string_of_value ~extended_lambda:false));
            eval_lambda ("~" ^ var) env body
        | Closure (s, env, Lambda (var, body)) ->
            let env = Hashtbl.copy env in
            logf
              "[Shunting_yard:eval_op:Apply:Closure] extending env of clo<%s> \
               by [%s : %s]\n"
              s var (string_of_value rhs);
            Hashtbl.replace env var rhs;
            logf "[Shunting_yard:eval_op:Apply:Closure] env after: [%s]\n"
              (string_of_closure env (string_of_value ~extended_lambda:false));
            eval_lambda s env body
        | Builtin b -> eval_builtin b rhs
        | _ -> Yard.failwith @@ Errors.reportTypeError "Evaluable" lhs)
    | _ ->
        Yard.failwith
          ("This should be unreachable, encountered op: "
         ^ string_of_operator op)

  and eval_lambda var env body =
    logf "[Shunting_yard] called eval_lambda with clo<%s>.env: [%s]; body: %s\n"
      var
      (string_of_closure env (string_of_value ~extended_lambda:false))
      (string_of_tokenList ~limit:5 body);
    match body with
    | Id id :: Operator Binding :: body ->
        logf "[Shunting_yard:eval_lambda] no recursion this time on clo<%s>\n"
          var;
        let lambda = Lambda (id, body) in
        Yard.push_value @@ Closure (var, env, lambda)
    | _ ->
        logf "[Shunting_yard:eval_lambda] entering recursion of clo<%s>\n" var;
        let* () = Yard.push_assign @@ ClosureEnv (var, env) in
        let* () = eval_prepared body in
        let* () = remove_closure_env () in
        logf "[Shunting_yard:eval_lambda] recursion of clo<%s> exited\n" var;
        Yard.return ()

  and eval () =
    let rec iter () =
      let* () = Yard.push_op StackSeparator in
      let* () = Yard.set_tiktok None in
      let* () = eval_iter () in
      let* t = Yard.peek_token in
      match t with
      | EOF ->
          logf "[Shunting_yard:eval] ecountered EOF, finishing\n";
          Yard.return ()
      | _ ->
          logf "[Shunting_yard:eval] no EOF continuing\n";
          iter ()
    in
    logf "[Shunting_yard] called eval\n";
    let* () = Yard.push_value ScopeBorder in
    let* () = iter () in
    deref_stack ()

  and eval_one () =
    logf "[Shunting_yard] called eval_one\n";
    let* () = Yard.push_op StackSeparator in
    let* () = Yard.set_tiktok None in
    let* () = eval_iter () in
    let* () = finish_eval () in
    pop_derefed ()

  and eval_prepared prepared =
    logf "[Shunting_yard] called eval_prepared with %s\n"
    @@ string_of_tokenList prepared;
    let+ () = (prepared, eval) in
    logf "[Shunting_yard:eval_prepared] finished evaling\n";
    Yard.return ()

  and remove_closure_env () =
    logf "[Shunting_yard] called remove_closure_env\n";
    let* assign = Yard.pop_assign in
    match assign with
    | ClosureEnv _ -> Yard.return ()
    | _ ->
        let* () = remove_closure_env () in
        Yard.push_assign assign

  and pop_derefed () =
    logf "[Shunting_yard] called pop_derefed\n";
    let* var = Yard.pop_value in
    deref_val var

  and deref_val var =
    logf "[Shunting_yard] called deref_val with %s\n" @@ string_of_value var;
    match var with
    | Id id -> (
        let* assignment_stack = Yard.assignment_stack in
        let pred = function
          | Assign (s, v) when s = id -> Some v
          | ClosureEnv (_, env) -> Hashtbl.find_opt env id
          | Assign _ -> None
          | ScopeBorder -> None
        in
        match List.find_map pred assignment_stack with
        | Some v ->
            logf "[Shunting_yard:deref_val] deref_val evaluated %s to %s\n" id
              (string_of_value v);
            Yard.return v
        | None -> Yard.failwith ("Referenced undefined variable: " ^ id))
    | _ -> Yard.return var

  and close_bracket () =
    logf "[Shunting_yard] called close_bracket\n";
    let* op = Yard.pop_op in
    match op with
    | OpenBracket -> Yard.return ()
    | _ ->
        let* () = eval_op op in
        close_bracket ()

  and try_push_op op =
    logf "[Shunting_yard] called try_push_op with %s\n" @@ string_of_operator op;
    let* top_op = Yard.peek_op in
    if
      operator_prio top_op > operator_prio op
      || (operator_prio top_op = operator_prio op && operator_binding op = Left)
    then
      let* top_op = Yard.pop_op in
      let* () = eval_op top_op in
      try_push_op op
    else Yard.push_op op

  and finish_eval () =
    logf "[Shunting_yard] called finish_eval\n";
    let* top_op = Yard.pop_op in
    if top_op = StackSeparator then Yard.return ()
    else if top_op = OpenBracket then
      Yard.failwith @@ Errors.reportUnclosedBracketWhileFolding
    else
      let* () = eval_op top_op in
      finish_eval ()

  and deref_stack () =
    logf "[Shunting_yard] called deref_stack\n";
    let* top_op = Yard.pop_op in
    if top_op = StackSeparator then (
      logf "[Shunting_yard:deref_stack] encountered StackSeparator\n";
      let* value = pop_derefed () in
      if value = ScopeBorder then (
        logf
          "[Shunting_yard:deref_stack] encountered ScopeBorder on the first \
           iteration\n";
        Yard.return ())
      else
        let* next_value = Yard.pop_value in
        let* () =
          if next_value = ScopeBorder then (
            logf
              "[Shunting_yard:deref_stack] encountered ScopeBorder, ending \
               recursion\n";
            Yard.return ())
          else (
            logf "[Shunting_yard:deref_stack] entering recursion\n";
            let* () = Yard.push_value next_value in
            deref_stack ())
        in
        Yard.push_value value)
    else if top_op = OpenBracket then
      Yard.failwith @@ Errors.reportUnclosedBracketWhileFolding
    else
      let* () = eval_op top_op in
      deref_stack ()

  and close_scope () =
    logf "[Shunting_yard] called close_scope\n";
    let* assign = Yard.pop_assign in
    match assign with
    | ScopeBorder -> Yard.return ()
    | ClosureEnv _ ->
        let* () = close_scope () in
        Yard.push_assign assign
    | Assign _ -> close_scope ()

  and handle_semicolon () =
    logf "[Shunting_yard] called handle_semicolon\n";
    let* () = finish_eval () in
    logf "[Shunting_yard:handle_semicolon] poping value\n";
    let* _ = Yard.pop_value in
    Yard.set_tiktok None

  and handle_binding () =
    logf "[Shunting_yard] called handle_binding\n";
    let* binded = Yard.pop_value in
    let* body = prepare () in
    match binded with
    | Id var -> Yard.push_value @@ Lambda (var, body)
    | _ -> Yard.failwith @@ Errors.reportTypeError "Id" binded
end
