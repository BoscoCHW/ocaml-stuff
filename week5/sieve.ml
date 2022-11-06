(* return x such that x * x <= n and (x+1) * (x+1) > n *)
let int_sqrt n = 
  n |> float_of_int |> sqrt |> int_of_float

let sieve n =
  let isprime = Array.make n true in 
  for i = 2 to int_sqrt n do 
    if isprime.(i) then
      let j = ref i in 
      while !j * i < n do 
        isprime.(!j * i) <- false;
        incr j 
      done 
  done;
  (* 0 and 1 are not prime *)
  isprime.(0) <- false; 
  isprime.(1) <- false;
  isprime |> Array.to_seqi |> List.of_seq |> List.filter_map (fun (i, b) -> if b then Some i else None)