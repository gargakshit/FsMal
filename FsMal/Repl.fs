namespace FsMal

open System

module Repl =
    open Colors
    open Printer
    open Reader
    open Types

    let read = readString

    let eval env ast = ast

    let rep str =
        match read str with
        | Ok form -> Ok(eval () form)
        | Error e -> Error e

    let uncoloredStdio prompt =
        (fun () -> ReadLine.Read prompt),
        (function
        | Ok form -> printfn $"%s{printToString true form}"
        | Error e -> printfn $"Error: %s{e}")

    let coloredStdio prompt =
        let coloredPrompt = (cyan >> bold) prompt
        let success = green
        let error = red
        let nil = (white >> dim) "nil"

        (fun () -> ReadLine.Read coloredPrompt),
        (function
        | Ok form ->
            match form with
            | Nil -> printfn $"%s{nil}"
            | Skip -> ()
            | _ -> printfn $"%s{success <| printToString true form}"
        | Error e -> printfn $"%s{error e}")

    let rec repl enableColors =
        let readline, writeline =
            (if enableColors then
                 coloredStdio
             else
                 uncoloredStdio)
                "user> "

        let input = readline ()
        input |> rep |> writeline
        ReadLine.AddHistory input

        // Loop
        repl enableColors
