(* Name; Heung Wai Chan (Bosco) A01258687 *)

(* Q1 *)
let min_elt l = List.fold_left (fun acc x -> if acc > x then x else acc) (List.hd l) l

let rec remove x = function
  | [] -> []
  | y::ys ->
    if x = y then ys
    else y::remove x ys

let selection_sort l = 
  let rec selection_sort' sorted l =
    match l with 
    | [] -> sorted
    | _ as l -> 
      let min  = min_elt l in
      selection_sort' (min::sorted) (remove min l)
  in

  List.rev (selection_sort' [] l)

(* Q2 *)
(* Given a list, group consecutive elements that are the same *)
let group l = 
  (* Given an accumulator (a nested list) and an element, 
    if the element is the same as the previous element, 
      add the element to the list inside the accumulator
    otherwise, add the element as a new list    
  *)
  let redux x acc =
    match acc with
    | [] -> [[x]]
    | _ -> 
      if (acc |> List.hd |> List.hd) = x 
      then (x::(List.hd acc))::List.tl acc
      else [x]::acc 
  in
  List.fold_right redux l []

let frequency l =
  (* acc: accumuator for frequency count 
     counter: the number of occurrence of the current item    
     l: a sorted list
     Given a sorted list, count the number of occurrence of each unique item
     and store it as a tuple in the accumulator
  *)
  let rec frequency' acc counter l = 
    match l with
    | [] -> acc
    | x1::x2::xs when x1 = x2 -> frequency' acc (counter + 1) (x2::xs)
    | x::xs-> frequency' ((x, counter)::acc) 1 xs
  in
  frequency' [] 1 (List.sort (fun a b -> if a > b then 1 else -1) l)