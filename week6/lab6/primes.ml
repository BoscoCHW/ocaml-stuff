(*Heung Wai Chan A01258687*)

type 'a infstream = Cons of 'a * (unit -> 'a infstream)

let rec from n = Cons (n, fun () -> from (n+1))

let rec take n (Cons (h, t)) =
  if n <= 0 then []
  else h::take (n-1) (t ())

(* Filters a stream based on predicate f *)
let rec filter f (Cons (hd, tl)) = 
  if f hd then Cons (hd, fun () -> filter f (tl ()))
  else filter f (tl ())

(* perform square root on an integer *)
let int_sqrt x = float_of_int x |> sqrt |> int_of_float

(* Determins if a number is prime *)
let isPrime x = 
  (* Determines if any integers from 2 to sqrt(x) is a factor of x *)
  let rec isFactorOfX factor = 
    if factor > int_sqrt x then false else 
    if x mod factor = 0 then true else
    isFactorOfX (factor + 1)
  in
  not (isFactorOfX 2)

let primes = filter isPrime (from 2)

let first100primes = take 100 primes