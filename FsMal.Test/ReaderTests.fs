namespace FsMal.Test

open FsMal.Reader
open FsMal.Types
open Xunit
open Xunit.Abstractions

type ReaderTests(output: ITestOutputHelper) =
    [<Fact>]
    let ``Reader returns Types.Skip on empty string`` () =
        let input = ""
        let expected = Ok Skip
        let actual = readString input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Reader returns Types.Skip on comments`` () =
        let input = "; This is a comment"
        let expected = Ok Skip
        let actual = readString input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Reader parses nil`` () =
        let input = "nil"
        let expected = Ok Nil
        let actual = readString input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Reader parses numbers`` () =
        let input = "12.34"
        let expected = Ok <| Number 12.34
        let actual = readString input

        Assert.Equal(expected, actual)

        let input' = "123"
        let expected' = Ok <| Number 123.0
        let actual' = readString input'

        Assert.Equal(expected', actual')

    [<Fact>]
    let ``Reader parses booleans`` () =
        let input = "true"
        let expected = Ok <| Bool true
        let actual = readString input

        Assert.Equal(expected, actual)

        let input' = "false"
        let expected' = Ok <| Bool false
        let actual' = readString input'

        Assert.Equal(expected', actual')

    [<Fact>]
    let ``Reader parses strings`` () =
        let input = "\"str\""
        let expected = Ok <| String "str"
        let actual = readString input

        Assert.Equal(expected, actual)

        let input' = "\"str \\\\ \\\" \\r\\n \\t\""
        let expected' = Ok <| String "str \\ \" \r\n \t"
        let actual' = readString input'

        Assert.Equal(expected', actual')

        let input'' = "\"str"
        let actual'' = readString input''

        match actual'' with
        | Ok _ -> Assert.True(false, $"Expected error, got {actual''}")
        | Error _ -> Assert.True(true)

    [<Fact>]
    let ``Reader parses lists`` () =
        let input =
            "(1   ,,2
        3 )"

        let expected =
            Ok(
                List [ Number 1.0
                       Number 2.0
                       Number 3.0 ]
            )

        let actual = readString input

        Assert.Equal(expected, actual)

        let input' = "( ,1\"abc\")"
        let expected' = Ok(List [ Number 1.0; String "abc" ])
        let actual' = readString input'

        Assert.Equal(expected', actual')

        let input'' = "( 1 2"
        let actual'' = readString input''

        match actual'' with
        | Ok _ -> Assert.True(false, $"Expected error, got {actual''}")
        | Error _ -> Assert.True(true)

    [<Fact>]
    let ``Reader parses vectors`` () =
        let input =
            "[1   ,,2
        3  ]"

        let expected =
            Ok(
                Vector [| Number 1.0
                          Number 2.0
                          Number 3.0 |]
            )

        let actual = readString input

        Assert.Equal(expected, actual)

        let input' = "[ ,1\"abc\"]"

        let expected' =
            Ok(Vector [| Number 1.0; String "abc" |])

        let actual' = readString input'

        Assert.Equal(expected', actual')

        let input'' = "[ 1 2"
        let actual'' = readString input''

        match actual'' with
        | Ok _ -> Assert.True(false, $"Expected error, got {actual''}")
        | Error _ -> Assert.True(true)

    [<Fact>]
    let ``Reader parses keyword`` () =
        let input = ":keyword"
        let expected = Ok(Keyword "keyword")
        let actual = readString input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Reader parses quote`` () =
        let input = "'quoted"

        let expected =
            Ok(List [ Symbol "quote"; Symbol "quoted" ])

        let actual = readString input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Reader parses quasiquote`` () =
        let input = "`quoted"

        let expected =
            Ok(
                List [ Symbol "quasiquote"
                       Symbol "quoted" ]
            )

        let actual = readString input

        output.WriteLine $"{actual}"

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Reader parses splice-unquote`` () =
        let input = "~@unquoted"

        let expected =
            Ok(
                List [ Symbol "splice-unquote"
                       Symbol "unquoted" ]
            )

        let actual = readString input

        output.WriteLine $"{actual}"

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Reader parses unquote`` () =
        let input = "~unquoted"

        let expected =
            Ok(
                List [ Symbol "unquote"
                       Symbol "unquoted" ]
            )

        let actual = readString input

        output.WriteLine $"{actual}"

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Reader parses deref`` () =
        let input = "@derefed"

        let expected =
            Ok(
                List [ Symbol "deref"
                       Symbol "derefed" ]
            )

        let actual = readString input

        output.WriteLine $"{actual}"

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Reader parses symbol`` () =
        let input = "symbol"
        let expected = Ok(Symbol "symbol")
        let actual = readString input

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Reader parses hashmaps`` () =
        let input =
            "{ :hello   ,,567
        \"abc\" 123  }"

        let expected =
            Ok(
                HashMap(
                    (Map.ofList [ "hello", Number 567.0 ]),
                    (Map.ofList [ "abc", Number 123.0 ])
                )
            )

        let actual = readString input

        Assert.Equal(expected, actual)

        let input' = "{ 1 2 }"
        let actual' = readString input'

        match actual' with
        | Ok _ -> Assert.True(false, $"Expected error, got {actual'}")
        | Error _ -> Assert.True(true)

        let input'' = "{ :a 1"
        let actual'' = readString input''

        match actual'' with
        | Ok _ -> Assert.True(false, $"Expected error, got {actual''}")
        | Error _ -> Assert.True(true)
