namespace FsMal

open System

module Repl =
    let read str = str

    let eval env ast = ast

    let print exp = exp

    let rep str = str |> read |> eval () |> print

    let rec repl () =
        // Get the input from stdin
        let input = ReadLine.Read "user> "

        // Evaluate
        input |> rep |> printfn "%s"

        // Add the expression to the readline history
        ReadLine.AddHistory input

        // Loop
        repl ()
