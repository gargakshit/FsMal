namespace FsMal

module Program =
    [<EntryPoint>]
    let main _argv =
        Repl.repl true

        0
