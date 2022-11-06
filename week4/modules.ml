(* a module M with function f and g*)
module M = struct 
  let f x = x * x
  let g x = x * x * x
end


module type M_sig = sig
  val f : int -> int
end
(* M2 only has function f*)
module M2 = (M: M_sig)


module type X_sig = sig
  val g : int -> int
end

(* M3 only has function g*)
module M3 = (M: X_sig)