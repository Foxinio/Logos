module type S = sig
  type elem

  val default : elem
end

module Make (M : S) = struct
  type t = M.elem list

  let default = M.default
  let hd = function [] -> M.default | x :: _ -> x
  let tl = function [] -> [] | _ :: xs -> xs
  let cons elem lst = elem :: lst
  let unpack lst = (hd lst, tl lst)

  let fold_left fn init =
    let rec iter acc lst = match unpack lst with
      | (x, []) -> fn acc x
      | (x, xs) -> iter (fn acc x) xs
    in
    iter init

  let rec fold_right fn init lst =
    match unpack lst with
      | (x, []) -> fn init x
      | (x, xs) -> fn (fold_right fn init xs) x

  let rec map fn lst =
      match unpack lst with
      | (x, []) -> [ fn x ]
      | (x, xs) -> (fn x) :: map fn xs

  let rec nth n lst =
      match unpack lst with
      | (x, xs) when xs = [] || n = 0 -> x
      | (_, xs) -> nth (n-1) xs
      
end
