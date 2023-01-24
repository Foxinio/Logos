
module Make(State : sig type t end) : sig
  type 'a t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  val get : State.t t
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
