# LOGOS

Logos is my pet project dynamically-typed, interpreted, functional language.

Language specyfications are in docs.md.
Example code is in `test/example_programs`.

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

Passing files as other arguments results in evaluating files and printing result to stdout.

Order of passed filese determines what definitions are visible at which point of evaluation.


## Future Plans
[x] Make environment persistant - evaluated identifiers stay visible to code evaluated after them.
[ ] Implement module system - keyword import/include <module-name/path> that finds file, 
    evaluates it and appends definition to assignment stack.
