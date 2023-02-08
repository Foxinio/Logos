open Projekt.Lexing_types
open Projekt.Lexing_utils

let display_results_detailed v =
  let rec iter v =
    match v with
    | Number n -> string_of_int n
    | Unit -> "()"
    | Bool b -> string_of_bool b
    | Id id -> id
    | Pair ((Pair _ as p1), v2) -> Printf.sprintf "(%s),%s" (iter p1) (iter v2)
    | Pair (v1, v2) -> Printf.sprintf "%s,%s" (iter v1) (iter v2)
    | Lambda (var, body) -> "<lambda:" ^ iter_lambda var body ^ ">"
    | Closure (env, value) -> "<closure:["^ string_of_closure env iter ^ "]:" ^ iter value ^ ">"
    | Builtin b -> "<builtin:"^string_of_builtin b^">"
    | ScopeBorder -> "<Internal_error>"
  and iter_lambda var (body : token list) =
    var ^ "->"
    ^
    match body with
    | Id id :: Operator Binding :: body -> iter_lambda id body
    | _ -> "(...)"
  in
  match v with Pair _ -> "(" ^ iter v ^ ")" | _ -> iter v


let display_results v =
  let rec iter v =
    match v with
    | Number n -> string_of_int n
    | Unit -> "()"
    | Bool b -> string_of_bool b
    | Id id -> id
    | Pair ((Pair _ as p1), v2) -> Printf.sprintf "(%s),%s" (iter p1) (iter v2)
    | Pair (v1, v2) -> Printf.sprintf "%s,%s" (iter v1) (iter v2)
    | Lambda _ -> "<lambda>"
    | Closure _ -> "<closure>"
    | Builtin _ -> "<builtin>"
    | ScopeBorder -> "<Internal_error>"
  in
  match v with Pair _ -> "(" ^ iter v ^ ")" | _ -> iter v

