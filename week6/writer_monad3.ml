(* monoid: something that can give you an identity [] @ l -> l or 0 + num -> num *)

let return x = (x, 0)

let bind (x, n) f = 
  let (x', n') = f x in 
  (x', n + n')

let ( >>= ) = bind

let ( let* ) = bind

let ( >> ) m1 m2 = 
  m1 >>= fun _ -> m2

(* ((), l) >> (x, l') -> (x, l @ l')*)

let inc x = (x + 1, x + 1)  (* what ? *)
let dec x = (x - 1, x - 1)

(* let rec gcd a b =
  if a mod b = 0 then b
  else gcd b (a mod b)

let tell msg = ((), [msg]) *)

(* let rec gcd' a b =
  if a mod b = 0 then (tell ("gcd is " ^ string_of_int b)) >> return b
  else tell (Printf.sprintf "%d mod %d = %d" a b (a mod b)) >> gcd' b (a mod b) *)


