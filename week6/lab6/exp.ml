(*Heung Wai Chan A01258687*)

type 'a lazystream = Cons of 'a * 'a lazystream Lazy.t

(* Take the first 10 terms of a lazystream *)
let rec take n (Cons (h, t)) =
  if n <= 0 then []
  else h::(take (n-1) @@ Lazy.force t)

(* create lazy stream from function f
   f: a function that transform a seed into an element of the stream and the next seed
   seed: the seed for the stream *)
let rec unfold f seed =
  let (v, seed') = f seed in 
  Cons (v, lazy (unfold f seed'))

(* First implementation of exp_terms *)
let exp_terms x = 
  (* Calculates the next term given 
     the index of the next term (starts from index 0) and 
     the current term. *)
  let next i term = term *. x /. float_of_int i in 
  (* Creates the stream starting from term 0 *)
  let rec from i n = Cons (n, lazy(from (i+1) (next (i+1) n))) in
  from 0 1.

(* Second implementation of exp_terms using unfold *)
let exp_terms' x =
  let next i term = term *. x /. float_of_int i in 
  let f (i, x) = (x, (i+1, next (i+1) x)) in
  unfold f (0, 1.)

let exp1point1 = List.fold_left (fun acc x -> acc +. x) 0. (take 20 @@ exp_terms 1.1)