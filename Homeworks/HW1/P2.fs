module P2

(* OCaml compatibility *)

module List = 
  let flatten l = List.fold (fun ac e -> ac@e) [] l

module String =
    let sub (str: string) s e = str.Substring(s,e)

let compose f g x = f (g x)

let flatten_map f = compose List.flatten (List.map f)

let rec flatten_map2 f a_list =
    match a_list with
        [] -> []
        | head::tail -> (f head) @ (flatten_map2 f tail)

let rec stutter lst =
    match lst with
        [] -> []
        | head::tail -> [head; head] @ (stutter tail)
let firsts ss = List.map (fun s -> String.sub s 0 1) ss


(* these tests should pass when done with Problem 2; each is true *)
let test1 = stutter ["hi"; "bye"] = ["hi"; "hi"; "bye"; "bye"]

(*
let test2 = firsts2 ["foo"; ""; ""; "bar"] = ["f"; ""; ""; "b"]
let test3 = remove_empties ["foo"; ""; ""; "bar"] = ["foo"; "bar"]
*)
