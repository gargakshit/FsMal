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
        let actual = printString true input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Printer prints number`` () =
        let input = Number 1.0
        let expected = "1.000000"
        let actual = printString true input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Printer prints string`` () =
        let input = String "\"Abc"
        let expected = "\"Abc"
        let actual = printString false input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Printer prints string in pretty mode`` () =
        let input = String "\"Abc\r\n\t"
        let expected = "\"\\\"Abc\\r\\n\\t\""
        let actual = printString true input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Printer prints bool`` () =
        let input = Bool true
        let expected = "true"
        let actual = printString true input

        Assert.Equal(expected, actual)

        let input' = Bool false
        let expected' = "false"
        let actual' = printString true input'

        Assert.Equal(expected', actual')

    [<Fact>]
    let ``Printer prints keyword`` () =
        let input = Keyword "key"
        let expected = ":key"
        let actual = printString true input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Printer prints symbol`` () =
        let input = Symbol "sym"
        let expected = "sym"
        let actual = printString true input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Printer prints atom`` () =
        let input = Atom(ref <| Number 1.0)
        let expected = "(atom 1.000000)"
        let actual = printString true input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Printer skips skip`` () =
        let input = Skip
        let expected = ""
        let actual = printString true input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Printer prints list`` () =
        let input =
            List [ String "Hello"
                   List [ Number 1.0;Bool true ] ]

        let expected = "(Hello (1.000000 true))"
        let actual = printString false input

        Assert.Equal(expected, actual)

        let input' =
            List [ String "Hello"
                   List [ Number 1.0;Bool true ] ]

        let expected' = "(\"Hello\" (1.000000 true))"
        let actual' = printString true input'

        Assert.Equal(expected', actual')

    [<Fact>]
    let ``Printer prints vector`` () =
        let input =
            Vector [| String "Hello"
                      Vector [| Number 1.0;Bool true |] |]

        let expected = "[Hello [1.000000 true]]"
        let actual = printString false input

        Assert.Equal(expected, actual)

        let input' =
            Vector [| String "Hello"
                      Vector [| Number 1.0;Bool true |] |]

        let expected' = "[\"Hello\" [1.000000 true]]"
        let actual' = printString true input'

        Assert.Equal(expected', actual')

    [<Fact>]
    let ``Printer prints compound forms`` () =
        let input =
            Vector [| String "Hello"
                      List [ Number 1.0
                             Vector [| Nil;Bool false |] ] |]

        let expected = "[Hello (1.000000 [nil false])]"
        let actual = printString false input

        Assert.Equal(expected, actual)

        let input' =
            Vector [| String "Hello"
                      List [ Number 1.0
                             Vector [| Nil;Bool false |] ] |]

        let expected' = "[\"Hello\" (1.000000 [nil false])]"
        let actual' = printString true input'

        Assert.Equal(expected', actual')
