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

let eval_file env channel =
  let lexbuf = Lexing.from_channel channel in
  let printer =
    if !Opts.print_detail then display_results_detailed else display_results
  in
  let lexed = lexer_to_list lexbuf in
  if !Opts.print_lexer then (
    Printf.printf "[[Lexing]]\n";
    to_string lexed string_of_token;
    Printf.printf "[[END]]\n");
  let (res, env) = Shunting_yard.run_with_env lexed env in
  to_string res printer;
  env

let eval_string env s =
  let lexed = lexer_to_list @@ Lexing.from_string s in
  let (res, env) = Shunting_yard.run_with_env lexed env in
  to_string res display_results;
  env

let rec repl env =
  Printf.printf "#";
  let line = try read_line () with End_of_file -> "exit" in
  if line = "exit" || line = "quit" then Printf.printf "\n"
  else (
    let env = eval_string env line in
    repl env)

let eval_files env files =
  let f env arg =
    try
      let channel = open_in arg in
      Printf.printf "%s:\n" arg;
      eval_file env channel
    with
    | Sys_error s -> Printf.eprintf "%s\n%!" s; []
    | exc -> Printf.eprintf "Other error: %s" @@ Printexc.to_string exc; []
  in
  List.fold_left f env !Opts.files

let eval () =
  let env =
    if List.length !Opts.to_eval > 0 then (
        List.fold_left (fun env expr -> eval_string env expr) [] !Opts.to_eval)
    else []
  in
  let env, b =
    let len = List.length !Opts.files in
    if len = 1 then (
      (try eval_file env @@ open_in @@ List.hd !Opts.files
      with Sys_error s -> Printf.eprintf "%s\n%!" s; []), true)
    else if len > 1 then (
      eval_files env !Opts.files, true)
    else env, false
  in
  if not b then repl env
