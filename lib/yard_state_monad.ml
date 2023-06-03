open Lexing_utils
open Lexing_types
open StateMonad
open Logf

module type S = sig
  type 'a t
  type yard

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  val eval_with : tokenList * (unit -> 'a t) -> ('a -> 'b t) -> 'b t
  (** pass tokenList and function to execute with it, then recover previous state *)

  val init : tokenList -> assignList -> 'a t -> 'a
  (** start monad with tokenList *)

  val read_token : token t
  (** pop token from list and return it *)

  val peek_token : token t
  (** read token from list without modifying it *)

  val push_value : value -> unit t
  (** push value to value stack *)

  val push_op : operator -> unit t
  (** push operator to operator stack *)

  val push_assign : assignment -> unit t
  (** push assignment to assignment stack *)

  val pop_value : value t
  (** pop value from the stack and return it *)

  val pop_op : operator t
  (** pop operator from the stack and return it *)

  val peek_op : operator t
  (** read operator from the stack without modifying it *)

  val pop_assign : assignment t
  (** pop assignment from the stack and return it *)

  val assignment_stack : assignList t
  (** view value stack *)

  val value_stack : valueList t
  (** view value stack *)

  val get_tiktok : bool option t
  (** get tiktok state *)

  val set_tiktok : bool option -> unit t
  (** set tiktok state *)

  val failwith : string -> 'a t
  (** failwith error *)

  val string_of_yard : yard -> string
end

let string_of_opt_bool = function
  | Some b -> "Some(" ^ string_of_bool b ^ ")"
  | None -> "None"

module Yard : S = struct
  type yard = {
    (* tiktok signals what came before,
       true means value,
       false means operator,
       none means nothing *)
    tiktok : bool option;
    value_stack : valueList;
    operator_stack : operatorList;
    assignment_stack : assignment list;
    token_iterator : tokenList list;
  }

  include StateMonad.Make (struct
    type t = yard
  end)

  let ( let* ) = bind

  let string_of_token_list token_iterator =
    to_string ~fold:No_fold token_iterator string_of_token

  let string_of_token_stack token_iterator =
    to_string ~line_prefix:"\t " ~fold:Force_fold (List.map string_of_token_list token_iterator)
      (fun s -> "[" ^ s ^ "]")

  let string_of_value_stack value_stack =
    to_string ~line_prefix:"\t " value_stack string_of_value

  let string_of_assign_stack assignment_stack =
    to_string ~line_prefix:"\t " assignment_stack string_of_assign

  let string_of_operator_stack operator_stack =
    to_string ~line_prefix:"\t " operator_stack string_of_operator

  let string_of_yard env =
    let token_string = string_of_token_stack env.token_iterator in
    let value_string = string_of_value_stack env.value_stack in
    let assignment_string = string_of_assign_stack env.assignment_stack in
    let operator_string = string_of_operator_stack env.operator_stack in
    Printf.sprintf
      "[Yard]=======================================\n\
       [Yard] State Dump:\n\
       [Yard] Token Stack:     [\n\
       \t %s]\n\
       [Yard] Operator Stack:  [\n\
       \t %s]\n\
       [Yard] Value Stack:     [\n\
       \t %s]\n\
       [Yard] Assignment Stack:[\n\
       \t %s]\n\
       [Yard]=======================================\n"
      token_string operator_string value_string assignment_string

  let dump_state =
    let* env = get in
    logf "%s\n" @@ string_of_yard env;
    return ()

  let failwith s =
    logf "[Yard] Failure: (%s)\n" s;
    let* () = dump_state in
    logf "[Yard]  Finishing with failure\n";
    Printf.eprintf "%s\n" s;
    log_backtrace ();
    close_log ();
    exit 1

  let check_stack_overflow =
    let* { operator_stack; _ } = get in
    if List.length operator_stack > 40 then failwith "operator stack overflow\n"
    else return ()

  let rec bind a b =
    let* res =
      try
        let* res = a in
        return res
      with e -> failwith @@ Printexc.to_string e
    in
    b res

  and init lexbuf env eval =
    let env =
      {
        value_stack = [];
        operator_stack = [];
        assignment_stack = env;
        token_iterator = [ lexbuf ];
        tiktok = None;
      }
    in
    let b =
      let* a = eval in
      let* () = dump_state in
      logf "[Yard] Finishing with success\n";
      return a
    in
    run env b

  and value_stack =
    let* { value_stack; _ } = get in
    logf "[Yard] called value_stack\n";
    return value_stack

  and assignment_stack =
    let* { assignment_stack; _ } = get in
    logf "[Yard] called assignment_stack\n";
    return assignment_stack

  and get_tiktok =
    let* { tiktok; _ } = get in
    logf "[Yard] called get_tiktok -> %s\n" @@ string_of_opt_bool tiktok;
    return tiktok

  and set_tiktok state =
    let* env = get in
    logf "[Yard] called set_tiktok with %s\n" @@ string_of_opt_bool state;
    set { env with tiktok = state }

  and eval_with (lst, trans) cont =
    logf "[Yard] called eval_with\n";
    let* ({ token_iterator; _ } as env) = get in
    let* () = set { env with token_iterator = lst :: token_iterator } in
    let* ret = trans () in
    let* env = get in
    let* () = set { env with token_iterator } in
    bind (return ret) cont

  and read_token =
    let* ({ token_iterator; _ } as env) = get in
    let token = TokenSeq.hd @@ List.hd token_iterator in
    let tail = (TokenSeq.tl @@ List.hd token_iterator) :: (List.tl token_iterator) in
    logf "[Yard] called read_token -> %s\n"
    @@ string_of_token  token;
    let* () = dump_state in
    let* () = set { env with token_iterator = tail } in
    return token

  and push_value x =
    let* ({ value_stack; _ } as env) = get in
    logf "[Yard] called push_value with %s\n" @@ string_of_value x;
    set { env with value_stack = x :: value_stack; tiktok = Some true }

  and push_op op =
    let* ({ operator_stack; _ } as env) = get in
    logf "[Yard] called push_op with %s\n" @@ string_of_operator op;
    let* () = check_stack_overflow in
    set { env with operator_stack = op :: operator_stack; tiktok = Some false }

  and push_assign assign =
    let* ({ assignment_stack; _ } as env) = get in
    logf "[Yard] called push_assign with %s\n" @@ string_of_assign assign;
    set { env with assignment_stack = assign :: assignment_stack }

  and pop_value =
    let* ({ value_stack; _ } as env) = get in
    logf "[Yard] called pop_value -> %s\n"
    @@ string_of_value @@ ValueSeq.hd value_stack;
    let* () = set { env with value_stack = ValueSeq.tl value_stack } in
    return @@ ValueSeq.hd value_stack

  and pop_op =
    let* ({ operator_stack; _ } as env) = get in
    logf "[Yard] called pop_op -> %s\n"
    @@ string_of_operator
    @@ OperatorSeq.hd operator_stack;
    let* () = dump_state in
    let* () = set { env with operator_stack = OperatorSeq.tl operator_stack } in
    return @@ OperatorSeq.hd operator_stack

  and pop_assign =
    let* ({ assignment_stack; _ } as env) = get in
    logf "[Yard] called pop_assign -> %s\n"
    @@ string_of_assign
    @@ AssignSeq.hd assignment_stack;
    let* () =
      set { env with assignment_stack = AssignSeq.tl assignment_stack }
    in
    return @@ AssignSeq.hd assignment_stack

  and peek_op =
    let* { operator_stack; _ } = get in
    logf "[Yard] called peek_op -> %s\n"
    @@ string_of_operator
    @@ OperatorSeq.hd operator_stack;
    return @@ OperatorSeq.hd operator_stack

  and peek_token =
    let* { token_iterator; _ } = get in
    let token = TokenSeq.hd @@ List.hd token_iterator in
    return token
end
