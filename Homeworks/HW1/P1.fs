module P1

module List = 
  let flatten l = List.fold (fun ac e -> ac@e) [] l
module String =
  let sub (str: string) s e = str.Substring(s,e)

let print_line (s: string) = System.Console.WriteLine s
let print_string (s: string) = System.Console.Write s

let string_of_int i = sprintf "%i" i
let string_of_bool (b: bool) = sprintf "%b" b

(* /OCaml compatibility *)

(* CSE P505, Autumn 2016, Homework 1 *)

type inttree = Empty | Node of int * inttree * inttree

(* use this function in fromList *)
let rec insert t i =
  match t with
    Empty -> Node(i,Empty,Empty)
  | Node(j,l,r) -> 
    if i=j then t 
    elif i < j then Node(j,insert l i,r)
    else Node(j,l,insert r i)

(* no need for this function; it is just an example *)
let rec contains t i =
  match t with
    Empty -> false
  | Node(j,l,r) -> i=j || (i < j && contains l i) || contains r i

let rec print t =
    match t with
        Empty -> ()
        | Node(j,l,r) ->
            print(l)
            print_string(sprintf "%i" j + ", ")
            print(r)

(* put fromList, sum1, prod1, avg1, map and negateAll here *)

let fromList int_list =
    List.fold insert Empty int_list

let rec sum1 tree =
    match tree with
      Empty -> 0
      | Node(j,l,r) ->
        sum1 l + j + sum1 r

let rec prod1 tree =
    match tree with
        Empty -> 1
        | Node(j,l,r) ->
            prod1 l * j * prod1 r

let rec map f tree =
    match tree with
        Empty -> Empty
        | Node(j,l,r) ->
          Node(f(j),map f l, map f r)


let rec fold f a t =
  match t with
    Empty -> a
  | Node(j,l,r) -> fold f (fold f (f a j) l) r

(* put your English answer to 1e here *)

(* put sum2, prod2, and avg2 here *)

(* a little testing for problem 1 -- 
   commented out since the functions do not exist yet 
   (You can/should write similar tests for other problems, but we won't
    grade your tests.) *)

(*
let tr = fromList [0;1;2;3;4;5;6;7;8;9;9;9;1] (* repeats get removed *)
let print_ans f t = print_string (string_of_int (f t)); print_string "\n"
let _ = print_ans sum1 tr
let _ = print_ans prod1 tr
let _ = print_ans avg1 tr
let _ = print_ans sum2 tr
let _ = print_ans prod2 tr
let _ = print_ans avg2 tr
*)
