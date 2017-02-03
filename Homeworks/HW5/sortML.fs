exception Unimplemented

let selection_sort l = raise Unimplemented

let merge_sort l = raise Unimplemented

let rec take n l = 
  if n <= 0 then [] else
    match l with
    | [] -> []
    | h::t -> h::take (n - 1) t

let sort_and_time k sorter l = 
  let stopWatch = System.Diagnostics.Stopwatch.StartNew()
  let first_k = (take k (sorter l)) in
  stopWatch.Stop()
  printf "%A\n" first_k
  stopWatch.Elapsed.TotalSeconds

let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

[<EntryPoint>]
let main(args) = 
  let nums = readLines args.[0] |> Seq.map System.Int32.Parse |> Seq.toList in
  let t1 = sort_and_time 5 selection_sort nums in
  let t2 = sort_and_time 5 merge_sort nums in
  printfn "Selection sort time %f" t1
  printfn "Merge sort time %f" t2
  0
