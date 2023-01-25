type location = Location of { line : int; column : int; file : string }

let string_of_location (Location { line; column; file }) =
  Format.sprintf "%s:%u:%u" file line column

type node_tag = NodeTag of int

let string_of_node_tag (NodeTag i) = Format.sprintf "%%node%i" i

(* Tworzy świeży identyfikator *)
let fresh_node_tag =
  let next = ref 0 in
  fun () ->
    let r = !next in
    next := r + 1;
    NodeTag r

exception InvalidToken of location * string

let mkLocation (pos, _) =
  let line = pos.Lexing.pos_lnum in
  let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
  let file = pos.Lexing.pos_fname in
  Location { line; column; file }

type relop = Equals | NotEquals | Less | Greater | LessEqual | GreaterEqual

let string_of_relop = function
  | Equals -> "=="
  | NotEquals -> "<>"
  | Less -> "<"
  | Greater -> ">"
  | LessEqual -> "<="
  | GreaterEqual -> ">="

type arthmop = Add | Sub | Mult | Div | Mod

let string_of_arthmop = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"

type boolop = Or | And | Not

let string_of_boolop = function Or -> "||" | And -> "&&" | Not -> "!"

type operator =
  | Semi
  | Assign
  | Binding
  | Apply
  | Comma
  | Relation of relop
  | Arthmetic of arthmop
  | Boolean of boolop
  | OpenBracket
  | CloseBracket
  | OpenScope
  | CloseScope
  | StackSeparator

module OperatorSeq = MySeq.Make (struct
  type elem = operator

  let default = StackSeparator
end)

type operatorList = OperatorSeq.t
type binding = Left | Right

let string_of_operator = function
  | Semi -> ";"
  | Assign -> "="
  | Binding -> "->"
  | Apply -> "$"
  | Comma -> ","
  | Relation rel -> string_of_relop rel
  | Arthmetic op -> string_of_arthmop op
  | Boolean op -> string_of_boolop op
  | OpenBracket -> "("
  | CloseBracket -> ")"
  | OpenScope -> "{"
  | CloseScope -> "}"
  | StackSeparator -> "<Stack_Separator>"

let operator_prio = function
  | StackSeparator -> -2
  | OpenBracket -> -1
  | Semi -> 0
  | Assign -> 1
  | Binding -> 2
  | Comma -> 3
  | Boolean Or -> 4
  | Boolean And -> 5
  | Boolean Not -> 6
  | Relation rel when rel = Equals || rel = NotEquals -> 7
  | Relation _ -> 8
  | Arthmetic op when op = Add || op = Sub -> 9
  | Arthmetic _ -> 10
  | Apply -> 11
  | _ as op ->
      raise
        (Invalid_argument
           (Printf.sprintf "operator %s has no priority"
           @@ string_of_operator op))

let operator_binding = function
  | Assign -> Right
  | Binding -> Right
  | Comma -> Right
  | Boolean _ -> Left
  | Relation _ -> Left
  | Arthmetic _ -> Left
  | Apply -> Left
  | _ as op ->
      raise
        (Invalid_argument
           (Printf.sprintf "operator %s has no binding rule"
           @@ string_of_operator op))

type builtin =
  | Fst
  | Snd
  | Readc
  | Writec
  | At
  | NumberPred
  | BoolPred
  | UnitPred
  | PairPred

let string_of_builtin = function
  | Fst -> "fst"
  | Snd -> "snd"
  | Readc -> "readc"
  | Writec -> "writec"
  | At -> "at"
  | NumberPred -> "is_number"
  | BoolPred -> "is_bool"
  | UnitPred -> "is_unit"
  | PairPred -> "is_pair"

type token =
  | Number of int
  | Operator of operator
  | Unit
  | Bool of bool
  | If
  | Id of string
  | Builtin of builtin
  | EOF

module TokenSeq = MySeq.Make (struct
  type elem = token

  let default = EOF
end)

type tokenList = TokenSeq.t

let string_of_token = function
  | Number n -> "Number(" ^ string_of_int n ^ ")"
  | Operator op -> "Operator(" ^ string_of_operator op ^ ")"
  | Unit -> "Unit()"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | If -> "If()"
  | Id id -> "Id(" ^ id ^ ")"
  | Builtin b -> "Builtin(" ^ string_of_builtin b ^ ")"
  | EOF -> "EOF"

type value =
  | Number of int
  | Bool of bool
  | Id of string
  | Unit
  | Pair of value * value
  | Lambda of string * TokenSeq.t
  | Closure of (string, value) Hashtbl.t * value

module ValueSeq = MySeq.Make (struct
  type elem = value

  let default = Unit
end)

type valueList = ValueSeq.t

let rec string_of_value = function
  | Number n -> "vNumber(" ^ string_of_int n ^ ")"
  | Bool b -> "vBool(" ^ string_of_bool b ^ ")"
  | Id id -> "vId(" ^ id ^ ")"
  | Unit -> "vUnit()"
  | Pair (v1, v2) ->
      "vPair(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"
  | Lambda (v, lst) ->
      let str =
        TokenSeq.fold_left (fun acc t -> acc ^ string_of_token t ^ ", ") "" lst
      in
      "vLambda(" ^ v ^ " -> [" ^ str ^ "]"
  | Closure (_, v) -> "vClosure(" ^ string_of_value v ^ ")"

let eval_binop lhs rhs = function
  | Add -> Number (lhs + rhs)
  | Sub -> Number (lhs - rhs)
  | Mult -> Number (lhs * rhs)
  | Div -> Number (lhs / rhs)
  | Mod -> Number (lhs mod rhs)

let eval_relop lhs rhs = function
  | Equals -> Bool (lhs = rhs)
  | NotEquals -> Bool (lhs <> rhs)
  | Less -> Bool (lhs < rhs)
  | Greater -> Bool (lhs > rhs)
  | LessEqual -> Bool (lhs <= rhs)
  | GreaterEqual -> Bool (lhs >= rhs)

type assignment = Assign of string * value | ScopeBorder

let string_of_assign = function
  | Assign (s, v) -> s ^ " : " ^ string_of_value v
  | ScopeBorder -> "{"

module AssignSeq = MySeq.Make (struct
  type elem = assignment

  let default = ScopeBorder
end)

type assignList = AssignSeq.t
