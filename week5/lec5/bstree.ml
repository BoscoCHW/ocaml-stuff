(* motivation for Maybe monad *)
type 'a bstree = L | N of 'a * 'a bstree * 'a bstree

let rec insert x t =
  match t with
  | L -> N (x, L, L)
  | N (x', l, r) when x < x' ->
    N (x', insert x l, r)
  | N (x', l, r) when x > x' ->
    N (x', l, insert x r)
  | _ -> t

let of_list l =
  List.fold_left (fun t x -> insert x t) L l

let left t =
  match t with
  | L -> None
  | N (_, l, _) -> Some l

let right t =
  match t with
  | L -> None
  | N (_, _, r) -> Some r

(* val bind : 'a option -> ('a -> 'b option) -> 'b option *)
let bind m f =
  match m with
  | None -> None
  | Some x -> f x

let ( >>= ) = bind

let return x = Some x

let ( >> ) m1 m2 = m1 >>= fun _ -> m2
