
type relop = Equals | NotEquals | Less | Greater | LessEqual | GreaterEqual


type arthmop = Add | Sub | Mult | Div | Mod


type boolop = Or | And | Not


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


type assignment =
  | Assign of string * value
  | ClosureEnv of (string, value) Hashtbl.t
  | ScopeBorder

module AssignSeq = MySeq.Make (struct
  type elem = assignment

  let default = ScopeBorder
end)

type assignList = AssignSeq.t
