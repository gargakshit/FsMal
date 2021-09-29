namespace FsMal

module Program =
    [<EntryPoint>]
    let main _argv =
        Repl.repl ()
        0
