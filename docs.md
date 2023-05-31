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

var := id

if-expr := "if" expr expr expr

pair-decl := expr "," expr

lambda-app := expr "$" expr

bool-expr :=
	| expr comp expr
	| bool-expr "&&" bool-expr
	| bool-expr "||" bool-expr
	| "!" bool-expr
	| "true"
	| "false"

arthm-expr :=
	| expr "+" expr
	| expr "-" expr
	| expr "\*" expr
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
	| "is_number" expr
	| "is_unit" expr
	| "is_bool" expr
	| "is_pair" expr




## Operator Prio:
In ascending order:
	;
	=
	->
	,
	||
	&&
	== <> 
	< > <= >=
	+ -
	* / %
    $
	!




## Operators and Builtins Meaning:

### + - / * %
normal arthmetic operations

### == <> < > <= >=
normal relation operations

### && || !
normal boolean operations

### ;
force evaluation, and pop one value from stack

### ->
lambda constructor, binds variable to the left and executes expr to the right

### =
assignment, forces evaluation of expression and assigns it to variable to the left
It also puts empty variable onto stack, to enable recursive lambdas

### ,
pair constructor, forces evaluation of expressions to both sides and creates pair from results
binds to the right, meaning: 
a , b , c 
means
a , (b , c)

### ()
constructs unit, type that has one value (similar to null)

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

### is\_number, is\_pair, is\_bool, is\_unit
predicates, say if value to the right is said type


## Value types:
- Number (int)
- Bool
- Unit
- Lambda 'a -> 'b
- Pair ('a * 'b)



## Language Semantics:

