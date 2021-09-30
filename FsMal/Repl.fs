namespace FsMal

open System

module Repl =
    open NerdyMishka
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
        | Ok form -> printfn $"%s{printString true form}"
        | Error e -> printfn $"Error: %s{e}")

    let coloredStdio prompt =
        let coloredPrompt = Chalk.Cyan().Bold().Draw(prompt)
        let success form = Chalk.BrightGreen().Draw(form)
        let error e = Chalk.BrightRed().Draw(e)
        let nil = Chalk.White().Dim().Draw("nil")

        (fun () -> ReadLine.Read coloredPrompt),
        (function
        | Ok form ->
            match form with
            | Nil -> printfn $"%s{nil}"
            |_ -> printfn $"%s{success <| printString true form}"
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
