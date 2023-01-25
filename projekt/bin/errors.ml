open Lexing_utils

let reportTypeError expected actual =
  Printf.sprintf "Type Error: expected %s, gotten %s" expected
    (string_of_value actual)

let reportUnexpectedToken expected actual =
  Printf.sprintf "Syntax Error: expected %s, gotten %s" expected
    (string_of_token actual)

let reportUnexpectedClosing of_what =
  Printf.sprintf "Syntax Error: Unexpected closing of unopened %s" of_what

let reportOperatorCumulation = "Syntax Error: operator cumulation"

let reportUnexpectedEOF expected =
  match expected with
  | Some expected ->
      Printf.sprintf "Syntax Error: Unexpected EOF, expected %s" expected
  | None -> Printf.sprintf "Syntax Error: Unexpected EOF"

let reportOperatorArgsTypeError expected actual1 actual2 =
  Printf.sprintf "Type Error: expected %s, gotten (%s, %s)" expected
    (string_of_value actual1) (string_of_value actual2)

let reportUnclosedBracketWhileFolding =
    "Syntax Error: Encountered unclosed bracket while folding the stack"

let reportDivisionByZero =
    "Execution Error: Division by zero"
