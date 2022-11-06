
(* Given a string of floating pointer numbers 
   separated by spaces, return an array of floats *)
let parse_floats s = 
  let stream = Scanf.Scanning.from_string s in 
  let rec do_parse acc = 
    match (Scanf.bscanf stream " %f" (fun x -> x ::acc)) with
    | xs -> do_parse xs
    | exception End_of_file -> acc
  in
  Array.of_list @@ List.rev @@ do_parse []

(* Given a filename, read the file containing a matrix of 
   n by n floating point numbers
   Return the matrix in float array array format *)
let read_distances filename = 
  let ic = open_in filename in
  let try_read () = try Some(input_line ic) with End_of_file -> None in
  
  let list_of_float_arr = 
    (* Read matrix file line by line, return a list of array of floats *)
    let rec loop acc = 
      match try_read () with 
      | Some s -> loop @@ parse_floats s::acc
      | None -> acc
    in
    List.rev @@ loop []
  in

  Array.of_list list_of_float_arr
  
(* Approximate solution for Travelling salesman problem using Annealing
   Read the matrix file and approximate shortest path through every city *)
let run filename (start_t, t_factor, t_interval) steps : float * int list = 
  let matrix = read_distances filename in 
  let distance c1 c2 = matrix.(c1).(c2) in
  (* Given a list of cities, find the length of the entire route *)
  let energy state = 
    let rec energy' prev_city cities acc =
      match cities with 
      | [] -> acc
      | hd::tl -> energy' hd tl ((distance prev_city hd) +. acc)
    in 
    let last_city = List.nth state (List.length state - 1) in
    energy' last_city state 0.0
  in 
  (* Given a list of cities, randomly swap two cities and return the list *)
  let next curr_state = 
    let arr = Array.of_list curr_state in 
    let len = Array.length arr in
    let random_int_1 = Random.int len in 
    let random_int_2 = Random.int len in
    let temp = arr.(random_int_1) in 
    arr.(random_int_1) <- arr.(random_int_2);
    arr.(random_int_2) <- temp;
    Array.to_list arr 
  in 
  let initial_state = List.init (Array.length matrix) (fun i -> i) in 

  let result = Anneal.run (initial_state, energy, next) (start_t, t_factor, t_interval) steps in 
  (energy result, result)
  
  