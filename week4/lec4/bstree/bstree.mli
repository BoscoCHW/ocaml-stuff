type 'a bstree = Leaf | Node of 'a * 'a bstree * 'a bstree
val bstree_insert : 'a -> 'a bstree -> 'a bstree
val bstree_of_list : 'a list -> 'a bstree
val bstree_size : 'a bstree -> int
val bstree_height : 'a bstree -> int
val bstree_find : 'a -> 'a bstree -> bool
val bstree_delete : 'a -> 'a bstree -> 'a bstree

