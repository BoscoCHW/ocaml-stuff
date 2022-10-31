let rec sum acc =
  try 
    sum (acc + Scanf.scanf " %d" (fun x -> x))
  with 
  | _ -> acc


let () = 
  Printf.printf "%d\n" (sum 0)