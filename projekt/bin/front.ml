open Projekt
open Lexer
open Lexing_utils
open Lexing_types
open Front_utils
module Opts = Argopts.Make ()
module Shunting_yard = Shunting_yard.Make (Yard_state_monad.Yard)

let to_string lst printer =
  let s = to_string lst printer in
  if s <> "" then Printf.printf "%s\n" s

let lexer_to_list lexbuf =
  let rec iter acc =
    let t = token lexbuf in
    match t with EOF -> List.rev acc | _ -> iter (t :: acc)
  in
  iter []

let eval_file channel =
  let lexbuf = Lexing.from_channel channel in
  let printer =
    if !Opts.print_detail then display_results_detailed else display_results
  in
  let lexed = lexer_to_list lexbuf in
  if !Opts.print_lexer then (
    Printf.printf "[[Lexing]]\n";
    to_string lexed string_of_token;
    Printf.printf "[[END]]\n");
  let res = Shunting_yard.run lexed in
  to_string res printer

let eval_string s =
  let lexed = lexer_to_list @@ Lexing.from_string s in
  let res = Shunting_yard.run lexed in
  to_string res display_results

let rec repl () =
  Printf.printf "#";
  let line = try read_line () with End_of_file -> "exit" in
  if line = "exit" || line = "quit" then Printf.printf "\n"
  else (
    eval_string line;
    repl ())

let eval_files files =
  let f () arg =
    try
      let channel = open_in arg in
      Printf.printf "%s:\n" arg;
      eval_file channel
    with
    | Sys_error s -> Printf.eprintf "%s\n%!" s
    | exc -> Printf.eprintf "Other error: %s" @@ Printexc.to_string exc
  in
  List.fold_left f () !Opts.files

let eval () =
  let b =
    if List.length !Opts.to_eval > 0 then (
      List.fold_left (fun () expr -> eval_string expr) () !Opts.to_eval;
      true)
    else false
  in
  let b =
    let len = List.length !Opts.files in
    if len = 1 then (
      (try eval_file @@ open_in @@ List.hd !Opts.files
       with Sys_error s -> Printf.eprintf "%s\n%!" s);
      true)
    else if len > 1 then (
      eval_files !Opts.files;
      true)
    else b
  in
  if not b then repl ()
