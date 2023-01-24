open Lexing_utils

let reportTypeError expected actual =
    Printf.sprintf "Type Error: expected %s, gotten %s\n" expected (string_of_value actual)

let reportUnexpectedToken expected actual =
    Printf.sprintf "Syntax Error: expected %s, gotten %s\n" expected (string_of_token actual)

let reportUnexpectedClosing of_what =
    Printf.sprintf "Syntax Error: Unexpected closing of unopened %s\n" of_what

let reportUnexpectedEOF expected =
    match expected with
    | Some(expected) ->
        Printf.sprintf "Syntax Error: Unexpected EOF, expected %s\n" expected
    | None    ->
        Printf.sprintf "Syntax Error: Unexpected EOF\n"


