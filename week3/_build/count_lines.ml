let rec count_lines acc = 
  try 
    ignore (read_line ());
    count_lines (acc + 1)
  with 
  | End_of_file -> acc

let () =
  Printf.fprintf (open_out "test") "Number of lines entered: %d\n" @@ count_lines 0