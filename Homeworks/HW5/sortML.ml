let selection_sort l = failwith "Unimplemented"

let merge_sort l = failwith "Unimplemented"

let rec read_all_lines f accum = 
  let r = try Some (input_line f) with End_of_file -> (close_in f; None) in
  match r with
  | None -> accum
  | Some l -> read_all_lines f (int_of_string l::accum)

let rec take n l = 
  if n <= 0 then [] else
    match l with
    | [] -> []
    | h::t -> h::take (pred n) t

let rec int_list_to_string l = 
  "[" ^ (String.concat "," (List.map string_of_int l)) ^ "]"

let sort_and_time k sorter l = 
  let startTime = Sys.time () in
  let first_k = take k (sorter l) in
  let endTime = Sys.time () in
  print_endline (int_list_to_string first_k);
  endTime -. startTime

let _ = 
  let f = open_in Sys.argv.(1) in
  let nums = read_all_lines f [] in
  let t1 = sort_and_time 5 selection_sort nums in
  let t2 = sort_and_time 5 merge_sort nums in
  print_endline ("Selection sort time " ^ (string_of_float t1));
  print_endline ("Merge sort time " ^ (string_of_float t2))
