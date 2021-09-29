let from whom = $"from %s{whom}"

[<EntryPoint>]
let main _argv =
    let message = from "F#"
    printfn $"Hello world %s{message}"
    0
