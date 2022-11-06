type student = {id: string; name: string; score: int}

let s1 = {id = "a11111111"; name = "homer simpson"; score = 35}

let s2 = {s1 with id = "a22222222"; name = "bart simpson"}

let name {id = i; name = n; score = s} = n

(*  
(* OR *)
let name {name = n} = n
(* OR *)
let name {name} = name  
*)

type instructor = {id: string; name: string; mutable salary: float}

let i1 = {id = "a66666666"; name = "monty burns"; salary = 1000000.}

(* i1.salary <- 100. *)

