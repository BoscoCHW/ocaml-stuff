(* Name: Heung Wai Chan (Bosco) A01258687*)

(* Q1 *)

let rec range1 a b = 
  if a > b then []
  else a::(range1 (a+1) b)

(* Reverse a given list. *)
let reverse l =
  let rec reverse' l res = 
    match l with 
    | [] -> res
    | x::xs -> reverse' xs (x::res)
  in 
  reverse' l []

let range a b =
  let rec range' a b res = 
    if a > b then res
    else range' (a+1) b (a::res)
  in
  reverse (range' a b [])

(* Drop first n elements in a list. *)
let rec drop n l =
  match l with 
  | [] -> []
  | _ when n < 1 -> l
  | x::xs -> drop (n-1) xs


(* Unzip a given list of paired elements *)
let unzip pair_list =
  let rec unzip' pair_list (list_a, list_b) =
    match pair_list with 
    | [] -> (list_a, list_b)
    | (a, b)::xs -> unzip' xs (a::list_a, b::list_b)
  in
  let (list_a, list_b) = unzip' pair_list ([], []) in
  (reverse list_a, reverse list_b)


let dedup l =
  let rec dedup' l res = 
    match l with
    | [] -> res
    | [x] -> x::res
    | first::second::rest ->
      if first = second then 
        dedup' (second::rest) res
      else
        dedup' (second::rest) (first::res )
  in 
  reverse (dedup' l [])


(* Q2 *)

(* Return the sum of first n terms of the speicified series. *)
let exp x n =
  (* Calculates the next term given the current term and 
     the index of the next term (starts from index 0) *)
  let next_term term i = term *. x /. float_of_int i in
  (* calculate the sum incrementally starting from index 0, keeping track of the 
     term to add and the answer. *)
  let rec exp' term ans i =
    if i = n then ans
    else exp' (next_term term (i+1)) (ans +. term) (i + 1)
  in
  exp' 1. 0. 0

