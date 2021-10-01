namespace FsMal.Test

open FsMal.Printer
open FsMal.Types
open Xunit
open Xunit.Abstractions

type PrinterTests(output: ITestOutputHelper) =
    [<Fact>]
    let ``Printer prints nil`` () =
        let input = Nil
        let expected = "nil"
        let actual = printToString true input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Printer prints number`` () =
        let input = Number 1.0
        let expected = "1"
        let actual = printToString true input

        Assert.Equal(expected, actual)

        let input' = Number 1.432832432
        let expected' = "1.432832432"
        let actual' = printToString true input'

        Assert.Equal(expected', actual')

    [<Fact>]
    let ``Printer prints string`` () =
        let input = String "\"Abc"
        let expected = "\"Abc"
        let actual = printToString false input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Printer prints string in pretty mode`` () =
        let input = String "\"Abc\r\n\t"
        let expected = "\"\\\"Abc\\r\\n\\t\""
        let actual = printToString true input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Printer prints bool`` () =
        let input = Bool true
        let expected = "true"
        let actual = printToString true input

        Assert.Equal(expected, actual)

        let input' = Bool false
        let expected' = "false"
        let actual' = printToString true input'

        Assert.Equal(expected', actual')

    [<Fact>]
    let ``Printer prints keyword`` () =
        let input = Keyword "key"
        let expected = ":key"
        let actual = printToString true input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Printer prints symbol`` () =
        let input = Symbol "sym"
        let expected = "sym"
        let actual = printToString true input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Printer prints atom`` () =
        let input = Atom(ref <| Number 1.0)
        let expected = "(atom 1)"
        let actual = printToString true input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Printer skips skip`` () =
        let input = Skip
        let expected = ""
        let actual = printToString true input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Printer prints list`` () =
        let input =
            List [ String "Hello"
                   List [ Number 1.0;Bool true ] ]

        let expected = "(Hello (1 true))"
        let actual = printToString false input

        Assert.Equal(expected, actual)

        let input' =
            List [ String "Hello"
                   List [ Number 1.0;Bool true ] ]

        let expected' = "(\"Hello\" (1 true))"
        let actual' = printToString true input'

        Assert.Equal(expected', actual')

    [<Fact>]
    let ``Printer prints vector`` () =
        let input =
            Vector [| String "Hello"
                      Vector [| Number 1.0;Bool true |] |]

        let expected = "[Hello [1 true]]"
        let actual = printToString false input

        Assert.Equal(expected, actual)

        let input' =
            Vector [| String "Hello"
                      Vector [| Number 1.0;Bool true |] |]

        let expected' = "[\"Hello\" [1 true]]"
        let actual' = printToString true input'

        Assert.Equal(expected', actual')

    [<Fact>]
    let ``Printer prints compound forms`` () =
        let input =
            Vector [| String "Hello"
                      List [ Number 1.0
                             Vector [| Nil;Bool false |] ] |]

        let expected = "[Hello (1 [nil false])]"
        let actual = printToString false input

        Assert.Equal(expected, actual)

        let input' =
            Vector [| String "Hello"
                      List [ Number 1.0
                             Vector [| Nil;Bool false |] ] |]

        let expected' = "[\"Hello\" (1 [nil false])]"
        let actual' = printToString true input'

        Assert.Equal(expected', actual')

    [<Fact>]
    let ``Printer prints hashmaps`` () =
        let input =
            HashMap(
                (Map.ofList [ "hello", Number 567.0 ]),
                (Map.ofList [ "abc", Number 123.0 ])
            )

        let expected = "{:hello 567 \"abc\" 123}"
        let actual = printToString true input

        Assert.Equal(expected, actual)

        let input' =
            HashMap((Map.ofList [ "hello", Number 567.0 ]), (Map.ofList []))

        let expected' = "{:hello 567}"
        let actual' = printToString true input'

        Assert.Equal(expected', actual')

    [<Fact>]
    let ``Printer prints built in functions`` () =
        let input = BuiltInFunc(0, (fun _ -> Nil))
        let expected = "#<function>"
        let actual = printToString true input

        Assert.Equal(expected, actual)
