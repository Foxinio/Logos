open Lexer
open Lexing_utils
open Lexing_types
module Opts = Argopts.Make ()
module Shunting_yard = Shunting_yard.Make (Yard_state_monad.Yard)

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
    | Pair ((Pair _ as p1), v2) -> Printf.sprintf "(%s),%s" (iter p1) (iter v2)
    | Pair (v1, v2) -> Printf.sprintf "%s,%s" (iter v1) (iter v2)
    | Lambda _ -> "<lambda>"
    | Closure _ -> "<closure>"
  in
  match v with Pair _ -> "(" ^ iter v ^ ")" | _ -> iter v

let to_string lst to_string =
  let s =
    List.fold_left
      (fun s t -> s ^ (if s = "" then "" else ", ") ^ to_string t)
      "" lst
  in
  if s <> "" then Printf.printf "%s\n" s

let eval_file lexbuf =
  if !Opts.extra_debug then (
    Printf.printf "[[Lexing]]\n";
    let lexed = lexer_to_list lexbuf in
    to_string lexed string_of_token;
    Printf.printf "[[END]]\n";
    Printf.printf "[[Executing]]\n";
    let res = Shunting_yard.run lexed in
    Printf.printf "result of len: %d\n" @@ List.length res;
    Printf.printf "[[END]]\n";
    Printf.printf "[[Printing result]]\n";
    to_string res print_stack;
    Printf.printf "[[END]]\n")
  else
    let lexed = lexer_to_list lexbuf in
    let res = Shunting_yard.run lexed in
    to_string res print_stack

let readline () =
    let rec iter acc =
        let res = Scanf.scanf "%c" Fun.id in
        if res = '\n' then acc else iter @@ Printf.sprintf "%s%c" acc res
    in iter ""

let rec repl () =
  Printf.printf "#";
  let line = try read_line () with End_of_file -> "exit" in
  if line = "exit" || line = "quit" then Printf.printf "\n"
  else
    let lexed = lexer_to_list @@ Lexing.from_string line in
    let res = Shunting_yard.run lexed in
    to_string res print_stack;
    repl ()

let eval_files () =
  Opts.parse_argv ();
  let argc = List.length !Opts.files in
  if argc = 0 then (
    Printf.printf "Using STD\n";
    repl ())
  else if argc = 1 then
    let channel = open_in @@ List.hd !Opts.files in
    let lexbuf = Lexing.from_channel channel in
    eval_file lexbuf
  else
    let f () arg =
      let channel = open_in arg in
      let lexbuf = Lexing.from_channel channel in
      Printf.printf "%s:\n" arg;
      eval_file lexbuf
    in
    List.fold_left f () !Opts.files
