open Projekt
open Lexing_types
open Lexing_utils
open Yard_state_monad

let pp_operator ppf op = Format.fprintf ppf "%s" @@ string_of_operator op

let pp_builtin ppf builtin =
  Format.fprintf ppf "%s" @@ string_of_builtin builtin

let pp_token ppf t = Format.fprintf ppf "%s" @@ string_of_token t
let pp_value ppf v = Format.fprintf ppf "%s" @@ string_of_value v
let pp_assign ppf a = Format.fprintf ppf "%s" @@ string_of_assign a
let pp_yard ppf env = Format.fprintf ppf "%s" @@ Yard.string_of_yard env
