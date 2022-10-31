let hd_opt l = 
  match l with 
  | [] -> None
  | x::_ -> Some x;;


let f = open_out "test" in
  let x = 123 in 
    Printf.fprintf f "The value is %d" x;;

(* should use a space before %d to remove leading white spaces*)
let x = Scanf.scanf " %d" (fun x -> x);;


let x = Scanf.scanf " %s" (fun x -> x);;
