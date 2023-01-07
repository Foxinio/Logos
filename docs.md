# Projekt (ewaluator Pseudo-logo):


## CFG:

prog := list(expr)

expr := 
	| expr ";"
	| assign
	| lambda
	| if-expr
	| pair-expr
	| lambda-app
	| arthm-expr
	| builtin
	| "{" expr "}"
	
assign := var "=" expr

lambda := var "->" expr

var := ":"id

if-expr := "if" expr expr expr

pair-decl := expr "," expr

lambda-app := var $ expr

arthm-expr :=
	| expr "+" expr
	| expr "-" expr
	| expr "*" expr
	| expr "/" expr
	| expr "%" expr
	| var
	| char
	| number
	| "()"
	| "(" arthm-expr ")"

builtin :=
	| "fst" expr
	| "snd" expr
	| "readc"
	| "writec" expr
	| "at" expr




## Operator Prio:
In ascending order:
	;
	=
	->
	,
	== <> 
	< > <= >=
	+ -
	* / %




## Operators and Builtins Meaning:

### + - / * %
normal arthmetic operations

### == <> < > <= >=
normal relation operations

### ;
force evaluation, and pop one value from stack

### ->
lambda constructor, binds variable to the left and executes expr to the right

### =
assignment, forces evaluation of expression and assigns it to variable to the left

### ,
pair constructor, forces evaluation of expressions to both sides and creates pair from results

### fst
takes pair and returns first value

### snd
takes pair and returns second value

### readc
reads char from stdin and evaluates to its ascii representation as number

### writec
takes number, prints it as ascii character to stdout and doesn't push anything to value stack

### at
takes number and treats it as index, coppies nth value from stack onto the top




## Value types:
- Number (float)
- Bool
- Unit
- Lambda 'a -> 'b
- Pair ('a * 'b)



## Language Semantics:

