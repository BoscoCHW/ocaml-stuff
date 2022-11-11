type 'a mylist = Nil | Cons of 'a * 'a mylist

exception Empty of string

let hd l =
  match l with 
  | Nil -> raise @@ Empty "hd"
  | Cons (x, _) -> x

let tl l =
  match l with 
  | Nil -> raise @@ Empty "tl"
  | Cons (_, t) -> t 

let rec take n l =
  if n <= 0 then []
  else 
    match l with 
    | Nil -> []
    | Cons (hd, tl) -> hd::(take (n-1) tl)

let rec map f l =
  match l with 
  | Nil -> Nil
  | Cons (hd, tl) -> Cons (f hd, map f tl)

