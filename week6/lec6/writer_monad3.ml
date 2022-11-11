let return x = (x, 0)

let bind (x, l) f =
  let (x', l') = f x in
  (x', l + l')

let ( >>= ) = bind

let ( let* ) = bind

let ( >> ) m1 m2 =
  m1 >>= fun _ -> m2

(* ((), l) >> (x, l') gives (x, l @ l') *)

let inc x = (x + 1, 1)
let dec x = (x - 1, 1)

(*
let rec gcd a b =
  if a mod b = 0 then b
  else gcd b (a mod b)

let tell msg = ((), [msg])

let rec gcd' a b =
  if a mod b = 0 then tell ("gcd is " ^ string_of_int b) >> return b
  else tell (Printf.sprintf "%d mod %d = %d" a b (a mod b)) >> gcd' b (a mod b)
*)
