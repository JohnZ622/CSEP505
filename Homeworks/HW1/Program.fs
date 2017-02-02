// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

(* OCaml compatibility *)

open P2

[<EntryPoint>]
let main argv = 
    printfn "%b" P2.test1
    0 // return an integer exit code
