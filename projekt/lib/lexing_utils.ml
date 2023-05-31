open Lexing_types
open Logf

type location = Location of { line : int; column : int; file : string }

let string_of_location (Location { line; column; file }) =
  Format.sprintf "%s:%u:%u" file line column

type node_tag = NodeTag of int

let string_of_node_tag (NodeTag i) = Format.sprintf "%%node%i" i

exception InvalidToken of location * string

let mkLocation (pos, _) =
  let line = pos.Lexing.pos_lnum in
  let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
  let file = pos.Lexing.pos_fname in
  Location { line; column; file }

type fold_opt = No_fold | Default | Force_fold

let to_string ?(line_prefix = "") ?(fold = Default) lst to_string =
  let lst = List.map to_string lst in
  let max, sum =
    List.fold_left
      (fun (max, sum) t ->
        let len = String.length t in
        (Int.max max len, len + sum))
      (0, 0) lst
  in
  let s =
    if ((max > 15 || sum > 50) && fold <> No_fold) || fold = Force_fold then
      List.fold_left
        (fun s t -> s ^ (if s = "" then "" else ",\n" ^ line_prefix) ^ t)
        "" lst
    else
      List.fold_left (fun s t -> s ^ (if s = "" then "" else ", ") ^ t) "" lst
  in
  s

let string_of_relop = function
  | Equals -> "=="
  | NotEquals -> "<>"
  | Less -> "<"
  | Greater -> ">"
  | LessEqual -> "<="
  | GreaterEqual -> ">="

let string_of_arthmop = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"

let string_of_boolop = function Or -> "||" | And -> "&&" | Not -> "!"

type binding = Left | Right

let string_of_operator = function
  | Semi -> ";"
  | Assign -> "="
  | Binding -> "->"
  | Apply -> "$"
  | Comma -> ","
  | Relation rel -> string_of_relop rel
  | Arthmetic op -> string_of_arthmop op
  | Boolean op -> string_of_boolop op
  | OpenBracket -> "("
  | CloseBracket -> ")"
  | OpenScope -> "{"
  | CloseScope -> "}"
  | StackSeparator -> "<Stack_Separator>"

let operator_prio = function
  | StackSeparator -> -2
  | OpenBracket -> -1
  | Semi -> 0
  | Assign -> 1
  | Binding -> 2
  | Comma -> 3
  | Boolean Or -> 4
  | Boolean And -> 5
  | Boolean Not -> 6
  | Relation rel when rel = Equals || rel = NotEquals -> 7
  | Relation _ -> 8
  | Arthmetic op when op = Add || op = Sub -> 9
  | Arthmetic _ -> 10
  | Apply -> 11
  | _ as op ->
      raise
        (Invalid_argument
           (Printf.sprintf "operator %s has no priority"
           @@ string_of_operator op))

let operator_binding = function
  | Binding -> Right
  | Assign -> Right
  | Comma -> Right
  | Boolean _ -> Left
  | Relation _ -> Left
  | Arthmetic _ -> Left
  | Apply -> Left
  | _ as op ->
      raise
        (Invalid_argument
           (Printf.sprintf "operator %s has no binding rule"
           @@ string_of_operator op))

let string_of_builtin = function
  | Fst -> "fst"
  | Snd -> "snd"
  | Readc -> "readc"
  | Writec -> "writec"
  | At -> "at"
  | NumberPred -> "is_number"
  | BoolPred -> "is_bool"
  | UnitPred -> "is_unit"
  | PairPred -> "is_pair"

let string_of_token = function
  | Operator op -> "Operator(" ^ string_of_operator op ^ ")"
  | Number n -> "Number(" ^ string_of_int n ^ ")"
  | Unit -> "Unit()"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | If -> "If()"
  | Id id -> "Id(" ^ id ^ ")"
  | Builtin b -> "Builtin(" ^ string_of_builtin b ^ ")"
  | EOF -> "EOF"

let string_of_tokenList ?(limit = 0) lst =
  let llimit = if limit <= 0 then List.length lst else limit in
  TokenSeq.fold_left
    (fun (i, acc) t ->
      if i > 0 then
        (i - 1, (if acc = "" then "" else acc ^ ", ") ^ string_of_token t)
      else if i = 0 then (-1, acc ^ ", ...")
      else (i, acc))
    (llimit, "") lst
  |> snd

let rec string_of_closure ?(max_depth = 5) env iter =
  Hashtbl.fold
    (fun k v acc ->
      let s =
        match v with
        | Closure (s, env1, _) when env1 == env -> "thisClosure(...)"
        | Closure (s, _, _) when max_depth = 0 ->
            k ^ " : maxClosure:" ^ s ^ "([...], (...))"
        | Closure (s, env, v) ->
            k ^ " : vClosure:" ^ s ^ "(["
            ^ string_of_closure ~max_depth:(max_depth - 1) env iter
            ^ "], " ^ iter v ^ ")"
        | _ -> k ^ " : " ^ iter v
      in
      if acc = "" then s else acc ^ ", " ^ s)
    env ""

let string_of_value ?(extended_lambda = true) v =
  let rec iter extended_lambda = function
    | Number n -> "vNumber(" ^ string_of_int n ^ ")"
    | Bool b -> "vBool(" ^ string_of_bool b ^ ")"
    | Id id -> "vId(" ^ id ^ ")"
    | Unit -> "vUnit()"
    | Pair (v1, v2) ->
        "vPair(" ^ iter extended_lambda v1 ^ ", " ^ iter extended_lambda v2
        ^ ")"
    | Lambda (v, lst) -> "vLambda(" ^ iter_lambda extended_lambda v lst ^ ")"
    | Closure (s, env, v) ->
        "vClosure:" ^ s ^ "(["
        ^ string_of_closure env (iter false)
        ^ "]," ^ iter extended_lambda v ^ ")"
    | Builtin b -> "vBuiltin(" ^ string_of_builtin b ^ ")"
    | ScopeBorder -> "vScope_Boarder()"
  and iter_lambda extended_lambda var (body : token list) =
    var ^ "->"
    ^
    match body with
    | Id id :: Operator Binding :: body -> iter_lambda extended_lambda id body
    | _ when extended_lambda -> "{" ^ string_of_tokenList ~limit:10 body ^ "}"
    | _ -> "{...}"
  in
  iter extended_lambda v

let string_of_assign = function
  | Assign (s, v) -> s ^ " : " ^ string_of_value v
  | ClosureEnv (s, env) ->
      "Env:" ^ s ^ "[" ^ string_of_closure env string_of_value ^ "]"
  | ScopeBorder -> "{"

let eval_binop lhs rhs = function
  | Add ->
      logf "[Shunting_yard:eval_op:eval_binop] evaluating %d+%d\n" lhs rhs;
      Number (lhs + rhs)
  | Sub ->
      logf "[Shunting_yard:eval_op:eval_binop] evaluating %d-%d\n" lhs rhs;
      Number (lhs - rhs)
  | Mult ->
      logf "[Shunting_yard:eval_op:eval_binop] evaluating %d*%d\n" lhs rhs;
      Number (lhs * rhs)
  | Div ->
      logf "[Shunting_yard:eval_op:eval_binop] evaluating %d/%d\n" lhs rhs;
      Number (lhs / rhs)
  | Mod ->
      logf "[Shunting_yard:eval_op:eval_binop] evaluating %d%%%d\n" lhs rhs;
      Number (lhs mod rhs)

let eval_relop lhs rhs = function
  | Equals ->
      logf "[Shunting_yard:eval_op:eval_relop] evaluating %d==%d\n" lhs rhs;
      Bool (lhs = rhs)
  | NotEquals ->
      logf "[Shunting_yard:eval_op:eval_relop] evaluating %d<>%d\n" lhs rhs;
      Bool (lhs <> rhs)
  | Less ->
      logf "[Shunting_yard:eval_op:eval_relop] evaluating %d<%d\n" lhs rhs;
      Bool (lhs < rhs)
  | Greater ->
      logf "[Shunting_yard:eval_op:eval_relop] evaluating %d>%d\n" lhs rhs;
      Bool (lhs > rhs)
  | LessEqual ->
      logf "[Shunting_yard:eval_op:eval_relop] evaluating %d<=%d\n" lhs rhs;
      Bool (lhs <= rhs)
  | GreaterEqual ->
      logf "[Shunting_yard:eval_op:eval_relop] evaluating %d>=%d\n" lhs rhs;
      Bool (lhs >= rhs)
