open Lexer
open Lexing_utils
open Printf
open Shunting_yard


module Shunting_yard = Make (Yard_state_monad.Yard)

let to_string lexbuf to_string =
  let s =
    List.fold_left
      (fun s t ->
        s ^ sprintf "%s%s" (if s = "" then "" else ", ") @@ to_string t)
      "" lexbuf
  in
  if s <> "" then printf "%s\n" s

let lexer_to_list lexbuf =
  let rec iter acc =
    let t = token lexbuf in
    match t with EOF -> List.rev acc | _ -> iter (t :: acc)
  in
  iter []

let print_stack v =
  let rec iter v =
    match v with
    | Number n -> string_of_int n
    | Unit -> "()"
    | Bool b -> string_of_bool b
    | Id id -> id
    | Pair (v1, v2) -> sprintf "%s,%s" (iter v1) (iter v2)
    | Lambda _ -> "<lambda>"
    | Closure _ -> "<closure>"
  in
  match v with Pair _ -> "(" ^ iter v ^ ")" | _ -> iter v

let main () =
  let lexbuf = Lexing.from_channel stdin in
  let lexed = lexer_to_list lexbuf in
  printf "[[Lexing]]\n";
  to_string lexed string_of_token;
  printf "[[END]]\n";
  printf "[[Executing]]\n";
  let res = Shunting_yard.run lexed in
  printf "result of len: %d\n" @@ List.length res;
  printf "[[END]]\n";
  printf "[[Printing result]]\n";
  to_string res print_stack;
  printf "[[END]]\n"

let () = main ()
let () = Logf.close_log ()
