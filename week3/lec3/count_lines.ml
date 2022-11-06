let rec count_lines acc =
  try
    ignore @@ read_line ();
    count_lines (acc + 1)
  with
  | End_of_file -> acc

let () =
  Printf.printf "%d\n" @@ count_lines 0
