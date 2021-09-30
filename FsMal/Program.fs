namespace FsMal

module Program =
    let parseAndPrint input =
        printfn $"%s{input}"

        match Reader.readString input with
        | Ok form -> printfn $"%O{form}"
        | Error err -> printfn $"%s{err}"

        printfn ""


    [<EntryPoint>]
    let main _argv =
        [ "(  1 [ 2.0    3.6328]     \"Hello World\"    nil  )"
          "\"This will error out"
          "; This is a comment"
          "symbol"
          "10.003"
          "[;
          1 ; one
          2     ,,3
          4      ]"
          ":i-am-a-keyword"
          "'quoted"
          "`quasiquoted"
          "~unquoted"
          "~@splice-unquoted"
          "@derefed"
          "( 1   2  )"
          "( ,1\"abc\")"
          "\"Hello\\\"\n\t\r World\"" ]
        |> Seq.iter parseAndPrint

        0
