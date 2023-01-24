open Lexing_utils

module Make(State : sig type t end) : sig
  type 'a t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  (** Pobierz stan *)
  val get : State.t t

  (** Ustaw stan *)
  val set : State.t -> unit t

  val run : State.t -> 'a t -> 'a

end = struct

  type 'a ans = State.t -> 'a * State.t
  type 'a t = { run : 'r. ('a -> 'r ans) -> 'r ans }

  let return x = { run = fun cont -> cont x }
  let bind m f = { run = fun cont ->
      m.run (fun x -> (f x).run cont) }

  let get = { run = fun cont s -> cont s s }
  let set s = { run = fun cont _ -> cont () s }

  let run s m = m.run (fun a s -> a, s) s |> fst
end

module type S = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  (** pass tokenList and function to execute with it, then recover previous state *)
  val eval_with : tokenList * (unit -> 'a t) -> (unit -> 'b t) -> 'b t

  (** start monad with tokenList *)
  val init : tokenList -> 'a t

  (** pop token from list and return it *)
  val read_token : token t

  (** read token from list without modifying it *)
  val peek_token : token t

  (** push value to value stack *)
  val push_value : value -> unit t

  (** push operator to operator stack *)
  val push_op : operator -> unit t

  (** push assignment to assignment stack *)
  val push_assign : assignment -> unit t

  (** pop value from the stack and return it *)
  val pop_value : value t

  (** pop operator from the stack and return it *)
  val pop_op : operator t

  (** read operator from the stack without modifying it *)
  val peek_op : operator t

  (** pop assignment from the stack and return it *)
  val pop_assign : assignment t

  (** view value stack *)
  val value_stack : valueList t

  (** get tiktok state *)
  val get_tiktok : bool t

  (** failwith error *)
  val failwith : string -> 'a t

end

module Yard : S = struct
  type yard = {
    tiktok : bool;
    value_stack : valueList;
    operator_stack : operatorList;
    assignment_stack : assignment list;
    token_iterator : tokenList;
  }
  include Make(struct type t = yard end)
  let ( let* ) = bind

  let make_env value_stack operator_stack assignment_stack token_iterator tiktok
      =
    { value_stack; operator_stack; assignment_stack; token_iterator; tiktok }

  let update_val new_val env = { env with value_stack = new_val }
  let update_op new_op env = { env with operator_stack = new_op }
  let update_assign new_assign env = { env with assignment_stack = new_assign }


  let eval_with (lst, trans) cont =
      let* { token_iterator; _ } as env = get in
      let* () = set { env with token_iterator=lst } in
      let* _ = trans () in
      let* env = get in
      bind (set { env with token_iterator }) cont
      


  let init lexbuf = run (make_env [] [] [] lexbuf false) eval 

  let value_stack =
      let* { value_stack; _ } = get in
      return value_stack

  let get_tiktok =
      let* { tiktok; _ } = get in
      return tiktok


  let read_token =
      let* { token_iterator; _ } as env = get in
      let* () = set { env with token_iterator = TokenSeq.tl token_iterator } in
      return @@ TokenSeq.hd token_iterator

  let push_value x =
      let* { value_stack; _ } as env = get in
      set { env with value_stack = x :: value_stack; tiktok = true }

  let push_op op =
      let* { operator_stack; _ } as env = get in
      set { env with operator_stack = op :: operator_stack; tiktok = false }

  let push_assign assign =
      let* { assignment_stack; _ } as env = get in
      set { env with assignment_stack = assign :: assignment_stack }


  let pop_value =
      let* { value_stack; _ } as env = get in
      let* () = set { env with value_stack = ValueSeq.tl value_stack } in
      return @@ ValueSeq.hd value_stack

  let pop_op =
      let* { operator_stack; _ } as env = get in
      let* () = set { env with operator_stack = OperatorSeq.tl operator_stack } in
      return @@ OperatorSeq.hd operator_stack

  let pop_assign =
      let* { assignment_stack; _ } as env = get in
      let* () = set { env with assignment_stack = AssignSeq.tl assignment_stack } in
      return @@ AssignSeq.hd assignment_stack



  let peek_op =
      let* { operator_stack; _ } = get in
      return @@ OperatorSeq.hd operator_stack

  let peek_token =
      let* { token_iterator; _ } = get in
      return @@ TokenSeq.hd token_iterator

  let failwith s = return @@ failwith s

end


