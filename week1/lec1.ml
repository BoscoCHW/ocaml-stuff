let rec fact n = 
  if n <= 0 then 1 
  else n * fact (n-1)

let fact2 n = 
  let rec fact' acc n = 
    if n <= 0 then acc
    else fact' (acc*n) (n-1)
  in 
  fact' 1 n

let mysqrt a = 
  let next x = 0.5 *. (x +. a /. x) in
  let good_enough x = abs_float(x *. x -. a) < 0.0000001 in 
  let rec iter x = 
    if good_enough x then x
    else iter (next x) 
  in
  iter 1.

let rec length l = 
  match l with
  | [] -> 0
  | _::xs -> 1 + length xs

let reverse l =
  let rec reverse' acc l =
    match l with 
    | [] -> acc
    | hd::tl -> reverse' (hd::acc) tl
  in 
  reverse' [] l

let rec take1 n l =
  match l with 
  | [] -> []
  | hd::tl -> 
    if n <= 0 then []
    else hd::take1 (n-1) tl


let rec take2 n l =
  match l with 
  | [] -> []
  | _ when n <= 0 -> [] (* using a guard *)
  | hd::tl -> hd::take2 (n-1) tl


let take3 n l =
  let rec take' acc n l =
    match l with 
    | [] -> acc
    | _ when n <= 0 -> acc
    | hd::tl -> take' (hd::acc) (n-1) tl
  in
  reverse (take' [] n l)



