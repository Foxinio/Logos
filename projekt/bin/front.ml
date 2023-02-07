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

let to_string lst to_string =
  let s =
    List.fold_left
      (fun s t -> s ^ (if s = "" then "" else ", ") ^ to_string t)
      "" lst
  in
  if s <> "" then Printf.printf "%s\n" s

let eval_file channel =
  let lexbuf = Lexing.from_channel channel in
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
    to_string res display_results_detailed;
    Printf.printf "[[END]]\n")
  else
    let lexed = lexer_to_list lexbuf in
    let res = Shunting_yard.run lexed in
    to_string res display_results

let rec repl () =
  Printf.printf "#";
  let line = try read_line () with End_of_file -> "exit" in
  if line = "exit" || line = "quit" then Printf.printf "\n"
  else
    let lexed = lexer_to_list @@ Lexing.from_string line in
    let res = Shunting_yard.run lexed in
    to_string res display_results;
    repl ()

let eval_files () =
  let argc = List.length !Opts.files in
  if argc = 0 then (
    repl ())
  else if argc = 1 then
    try eval_file @@ open_in @@ List.hd !Opts.files
    with Sys_error s -> Printf.eprintf "%s\n%!" s
  else
    let f () arg =
      try
        let channel = open_in arg in
        Printf.printf "%s:\n" arg;
        eval_file channel
      with Sys_error s -> Printf.eprintf "%s\n%!" s
    in
    List.fold_left f () !Opts.files
