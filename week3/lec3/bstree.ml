type 'a bstree =
  | Leaf 
  | Node of 'a * 'a bstree * 'a bstree

let rec bstree_insert x t =
  match t with
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (x', l, r) when x < x' -> Node (x', bstree_insert x l, r)
  | Node (x', l, r) when x > x' -> Node (x', l, bstree_insert x r)
  | _ -> t

let bstree_of_list l =
  List.fold_left (fun t x -> bstree_insert x t) Leaf l

let rec bstree_size t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + bstree_size l + bstree_size r

let rec bstree_height t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (bstree_height l) (bstree_height r)

let rec bstree_find x t =
  match t with
  | Leaf -> false
  | Node (x', l, r) when x < x' -> bstree_find x l
  | Node (x', l, r) when x > x' -> bstree_find x r
  | _ -> true

let rec bstree_max t =
  match t with
  | Leaf -> failwith "bstree_max: empty tree has no maximum"
  | Node (x, _, Leaf) -> x
  | Node (_, _, r) -> bstree_max r

let rec bstree_delete x t =
  match t with
  | Leaf -> Leaf
  | Node (x', l, r) when x < x' -> Node (x', bstree_delete x l, r)
  | Node (x', l, r) when x > x' -> Node (x', l, bstree_delete x r)
  | Node (_, Leaf, r) -> r
  | Node (_, l, Leaf) -> l
  | Node (_, l, r) ->
      let max = bstree_max l in
      Node (max, bstree_delete max l, r)
