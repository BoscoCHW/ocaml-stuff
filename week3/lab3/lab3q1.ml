(* Name; Heung Wai Chan (Bosco) A01258687 *)

let words () = 
  (* Recursively read words from input and append to accumulator*)
  let rec read_words acc = 
    let word = Scanf.scanf " %s" (fun x -> x) in
    match word with
    | "" -> acc 
    | _ -> read_words (word::acc)
  in
  List.rev @@ read_words []

let () = 
  let string_list = words() in
  (* check if the string is a number 
     add to accummulator if it's a number *)
  let reducer acc x = 
    let num = int_of_string_opt x in
    match num with 
    | None -> acc 
    | Some x -> acc + x
  in
  let sum = List.fold_left reducer 0 string_list in
  Printf.printf "The sum is %d.\n" sum
