(* Variants *)

type color = Red | Green | Blue | RGB of int * int * int

type 'a maybe = Nothing | Some of 'a

type 'a bstree = Leaf | Node of 'a * 'a bstree * 'a bstree

let t = Leaf;;
let t1 = Node (2, Leaf, Leaf);