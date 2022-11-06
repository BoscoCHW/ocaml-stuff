let return x = [x]

let bind l f = List.concat_map f l

let ( >>= ) = bind

let ( >> ) l1 l2 = l1 >>= fun _ -> l2

let guard cond l =
  if cond then l else []

let ( let* ) = ( >>= )

(* 2 dice summing to n *)
let sum_to n =
  List.init 6 ((+) 1) >>= fun x ->
  List.init 6 ((+) 1) >>= fun y ->
  if x + y = n then [(x, y)] else []

(* 2 positive integers whose product is n *)
let multiply_to n =
  List.init n ((+) 1) >>= fun x ->
  List.init n ((+) 1) >>= fun y ->
  guard (x * y = n) [(x, y)]

let multiply_to' n =
  let* x = List.init n ((+) 1) in
  let* y = List.init n ((+) 1) in
  guard (x * y = n) [(x, y)]
