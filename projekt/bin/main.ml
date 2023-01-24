open Lexer
open Lexing_utils
open Printf

let lexbuf = Lexing.from_channel stdin

let rec parse lexbuf = (
    let t = token lexbuf in
    printf "%s, " (string_of_token t);
    if t <> Lexing_utils.EOF then
        parse lexbuf
    else ())
    
let () = printf "[[Lexing]]\n"
let () = parse lexbuf
let () = printf "\n[[END]]\n"


