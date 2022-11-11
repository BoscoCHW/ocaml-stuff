let return x = (x, "")

let bind (x, s) f =
  let (x', s') = f x in
  (x', s ^ s')

let ( >>= ) = bind

let ( let* ) = bind

let ( >> ) m1 m2 =
  m1 >>= fun _ -> m2

(* ((), l) >> (x, l') gives (x, l @ l') *)

let inc x = (x + 1, "inc " ^ string_of_int x ^ "; ")
let dec x = (x - 1, "dec " ^ string_of_int x ^ "; ")

(*
let rec gcd a b =
  if a mod b = 0 then b
  else gcd b (a mod b)

let tell msg = ((), [msg])

let rec gcd' a b =
  if a mod b = 0 then tell ("gcd is " ^ string_of_int b) >> return b
  else tell (Printf.sprintf "%d mod %d = %d" a b (a mod b)) >> gcd' b (a mod b)
*)   
