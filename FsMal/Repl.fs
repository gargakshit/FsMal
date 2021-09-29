namespace FsMal

open System

module Repl =
    let read str = str

    let eval env ast = ast

    let print exp = exp

    let rep str = str |> read |> eval "" |> print
    
    let rec repl () =
        ReadLine.Read "user> "
        |> rep
        |> printfn "%s"
        
        repl ()
