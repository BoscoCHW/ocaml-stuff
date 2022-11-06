let () =
  for i = 1 to Array.length Sys.argv - 1 do
    Printf.printf "%s " Sys.argv.(i)
  done;
  Printf.printf "\n"

