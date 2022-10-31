let rec print_lines n =
  try
    let line = read_line () in
    Printf.printf "%d : %s : %d\n" n line (String.length line);
    print_lines (n + 1)
  with
  | End_of_file -> ()

let () = print_lines 1
