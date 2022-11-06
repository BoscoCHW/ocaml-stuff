(* type 'a t = Leaf | Node of 'a * 'a t * 'a t *)
type 'a t  (* abstract type *)

val empty : 'a t
val is_empty : 'a t -> bool
val insert : cmp:('a -> 'a -> int) -> 'a -> 'a t -> 'a t
val of_list : cmp:('a -> 'a -> int) -> 'a list -> 'a t
val size : 'a t -> int
val height : 'a t -> int
val find : cmp:('a -> 'b -> int) -> 'a -> 'b t -> bool
val delete : cmp:('a -> 'a -> int) -> 'a -> 'a t -> 'a t
