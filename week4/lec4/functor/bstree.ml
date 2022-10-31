module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t = Leaf | Node of elt * t * t
  val empty : t
  val is_empty : t -> bool
  val insert : elt -> t -> t
  val find : elt -> t -> bool
  val delete : elt -> t -> t
  val of_list : elt list -> t
end

module Make(Ord : OrderedType) = struct
  type elt = Ord.t
  type t = Leaf | Node of elt * t * t

  let empty = Leaf

  let is_empty t = t = Leaf

  let rec insert x t =
    match t with
    | Leaf -> Node (x, Leaf, Leaf)
    | Node (x', l, r) when Ord.compare x x' < 0 ->
      Node (x', insert x l, r)
    | Node (x', l, r) when Ord.compare x x' > 0 ->
      Node (x', l, insert x r)
    | _ -> t

  let rec find x t =
    match t with
    | Leaf -> false
    | Node (x', l, r) when Ord.compare x x' < 0 ->
      find x l
    | Node (x', l, r) when Ord.compare x x' > 0 ->
      find x r
    | _ -> true

  let rec rightmost t =
    match t with
    | Leaf -> failwith "rightmost: empty tree"
    | Node (x, _, Leaf) -> x
    | Node (_, _, r) -> rightmost r

  let rec delete x t =
    match t with
    | Leaf -> Leaf
    | Node (x', l, r) when Ord.compare x x' < 0 ->
      Node (x', delete x l, r)
    | Node (x', l, r) when Ord.compare x x' > 0 ->
      Node (x', l, delete x r)
    | Node (_, l, Leaf) -> l
    | Node (_, Leaf, r) -> r
    | Node (_, l, r) ->
      let x'' = rightmost l in
      Node (x'', delete x'' l, r)

  let of_list l =
    List.fold_left (fun t x -> insert x t) Leaf l
end
