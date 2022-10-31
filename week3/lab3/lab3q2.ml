(* Name; Heung Wai Chan (Bosco) A01258687 *)

type ('a, 'b) bstree =
  | Leaf 
  | Node of ('a * 'b) * ('a, 'b) bstree * ('a, 'b) bstree

let rec bstree_insert t k v=
  match t with
  | Leaf -> Node ((k, v), Leaf, Leaf)
  | Node ((k', v'), l, r) when k < k' -> Node ((k', v'), bstree_insert l k v, r)
  | Node ((k', v'), l, r) when k > k' -> Node ((k', v'), l, bstree_insert r k v)
  | Node ((k', v'), l, r) -> Node ((k', v), l, r)

let bstree_of_list l =
  List.fold_left (fun t (k, v) -> bstree_insert t k v) Leaf l

let rec bstree_size t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + bstree_size l + bstree_size r

let rec bstree_height t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (bstree_height l) (bstree_height r)

let rec bstree_find t k =
  match t with
  | Leaf -> None
  | Node ((k', v'), l, r) when k < k' -> bstree_find l k
  | Node ((k', v'), l, r) when k > k' -> bstree_find r k
  | Node ((k', v'), _, _) -> Some v'

let rec bstree_max t =
  match t with
  | Leaf -> failwith "bstree_max: empty tree has no maximum"
  | Node (x, _, Leaf) -> x
  | Node (_, _, r) -> bstree_max r

let rec bstree_delete t k =
  match t with
  | Leaf -> Leaf
  | Node ((k', v'), l, r) when k < k' -> Node ((k', v'), bstree_delete l k, r)
  | Node ((k', v'), l, r) when k > k' -> Node ((k', v'), l, bstree_delete r k)
  | Node (_, Leaf, r) -> r
  | Node (_, l, Leaf) -> l
  | Node (_, l, r) ->
      let (k', v') = bstree_max l in
      Node ((k', v'), bstree_delete l k', r)
