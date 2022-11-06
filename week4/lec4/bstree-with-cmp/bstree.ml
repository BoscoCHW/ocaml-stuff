type 'a t = Leaf | Node of 'a * 'a t * 'a t

let empty = Leaf

let is_empty t = t = Leaf

let rec insert ~cmp x t =
  match t with
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (x', l, r) when cmp x x' < 0 -> Node (x', insert ~cmp x l, r)
  | Node (x', l, r) when cmp x x' > 0 -> Node (x', l, insert ~cmp x r)
  | _ -> t

let of_list ~cmp l =
  List.fold_left (fun t x -> insert ~cmp x t) Leaf l

let rec size t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + size l + size r

let rec height t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (height l) (height r)

let rec find ~cmp x t =
  match t with
  | Leaf -> false
  | Node (x', l, r) when cmp x x' < 0 -> find ~cmp x l
  | Node (x', l, r) when cmp x x' > 0 -> find ~cmp x r
  | _ -> true

let rec rightmost t =
  match t with
  | Leaf -> failwith "max: empty tree has no maximum"
  | Node (x, _, Leaf) -> x
  | Node (_, _, r) -> rightmost r

let rec delete ~cmp x t =
  match t with
  | Leaf -> Leaf
  | Node (x', l, r) when cmp x x'< 0 -> Node (x', delete ~cmp x l, r)
  | Node (x', l, r) when cmp x x'> 0 -> Node (x', l, delete ~cmp x r)
  | Node (_, Leaf, r) -> r
  | Node (_, l, Leaf) -> l
  | Node (_, l, r) ->
      let max = rightmost l in
      Node (max, delete ~cmp max l, r)
