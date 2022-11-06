let rec list_equal (l1: 'a list) (l2: 'a list) : bool =  (* type annotation *)
  match l1, l2 with
  | [], [] -> true
  | [], _ | _, [] -> false
  | h1::_, h2::_ when h1 <> h2 -> false
  | _::t1, _::t2 -> list_equal t1 t2

let rec every_other = function
  | x1::x2::xs -> x1::every_other xs
  | l -> l

(* insertion sort *)
(* assume l is in ascending order *)  
let rec insert x = function
  | [] -> [x]
  | y::ys as l ->
      if x <= y then x::l
      else y::insert x ys

let rec remove x = function
  | [] -> []
  | y::ys ->
      if x = y then ys
      else y::remove x ys

let rec insertion_sort = function
  | [] -> []
  | x::xs -> insert x (insertion_sort xs)

let rec find_even = function
  | [] -> None
  | x::_ when x mod 2 = 0 -> Some x
  | _::xs -> find_even xs

(* val f : ('a -> bool) -> 'a list -> 'a option *) 
let rec find f = function
  | [] -> None
  | x::_ when f x -> Some x
  | _::xs -> find f xs

let rec map f = function
  | [] -> []
  | x::xs -> f x::map f xs

let rec filter f = function
  | [] -> []
  | x::xs when f x -> x::filter f xs
  | _::xs -> filter f xs

(* fold_left, fold_right!!! *)

(* val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
let rec fold_left f a l =
  match l with
  | [] -> a
  | x::xs -> fold_left f (f a x) xs

(* val fold_right : ('a -> 'b -> 'b)  -> 'a list -> 'b -> 'b *)
let rec fold_right f l a =
  match l with
  | [] -> a
  | x::xs -> f x (fold_right f xs a)
