# LOGOS

Logos is my pet project non-typed, interpreted language.

Language specyfications are in docs.md.

## Dependencies
`dune`
`opam`
`bmake`
`getopts` (from opam)

## Usage

Calling program without arguments enters repl.

flag `-e <expr>` evaluated expression and prints result to stdout.

flag `-l` prints lexer output.

flag `-g` prints extra debug.

passing files as other arguments results in evaluating files and printing result to stdout.


## Future Plans
Make environment persistant - evaluated identifiers stay visible to code evaluated after them.
