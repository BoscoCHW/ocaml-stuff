type 'a mylist = Nil | Cons of 'a * 'a mylist

exception Empty of string

let hd l =
  match l with
  | Nil -> raise (Empty "hd")
  | Cons (h, _) -> h

let tl l =
  match l with
  | Nil -> raise (Empty "tl")
  | Cons (_, t) -> t

let rec take n l =
  if n <= 0 then []
  else
    match l with
    | Nil -> []
    | Cons (h, t) -> h::take (n-1) t
            
let rec map f l =
  match l with
  | Nil -> Nil
  | Cons (h, t) -> Cons (f h, map f t)
