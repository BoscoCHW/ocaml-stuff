(* Heung Wai Chan A01258687 *)
let () =
  Random.init 32768;
  let filename = Sys.argv.(1) in 
  let num_steps = int_of_string Sys.argv.(2) in 
  let t_interval = num_steps / 500 + 1 in
  let (path_len, route) = Tsp.run filename (100., 0.99, t_interval) num_steps in 
  Printf.printf "Path length: %f\n" path_len;
  List.iter (fun x -> Printf.printf "%d " x) route;
  Printf.printf "\n";