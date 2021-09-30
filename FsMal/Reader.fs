namespace FsMal

module Reader =
    open FParsec
    open Types

    let private quote = Symbol "quote"
    let private quasiquote = Symbol "quasiquote"
    let private unquote = Symbol "unquote"
    let private spliceUnquote = Symbol "splice-unquote"
    let private deref = Symbol "deref"

    let private parseComment =
        parse {
            do! skipChar ';'
            do! restOfLine false |>> ignore

            return Skip
        }

    let private parseForm, private parseFormRef: Parser<Form, unit> * Parser<Form, unit> ref =
        createParserForwardedToRef ()

    let private parseNil = pstring "nil" |>> (fun _ -> Nil)

    let private parseFloat = pfloat |>> Number

    let private parseBool =
        (stringReturn "true" (Bool true))
        <|> (stringReturn "false" (Bool false))

    let private allowedChars = noneOf "\n\r \"(),;[\\]{}"

    let private parseSymbol = many1Chars allowedChars |>> Symbol

    let private parseKeyword =
        parse {
            do! skipChar ':'
            let! keyword = many1Chars allowedChars

            return Keyword keyword
        }

    let private parsePrefixed prefix =
        parse {
            do! skipString prefix
            return! parseForm
        }

    let private parseSpecial prefix symbol =
        parsePrefixed prefix
        |>> (fun form -> List [ symbol;form ])

    let private parseQuote = parseSpecial "'" quote
    let private parseQuasiQuote = parseSpecial "`" quasiquote
    let private parseUnquote = parseSpecial "~" unquote
    let private parseSpliceUnquote = parseSpecial "~@" spliceUnquote
    let private parseDeref = parseSpecial "@" deref

    let private parseString =
        let normalCharSnippet =
            manySatisfy (fun c -> c <> '\\' && c <> '"')

        let escapedChar =
            pstring "\\"
            >>. (anyOf "\\nrt\""
                 |>> function
                     | 'n' -> "\n"
                     | 'r' -> "\r"
                     | 't' -> "\t"
                     | c -> string c)

        between
            (pstring "\"")
            (pstring "\"")
            (stringsSepBy normalCharSnippet escapedChar)
        |>> String

    let private parseBetween openChar closeChar =
        parse {
            let sep =
                skipMany (
                    (anyOf ",\n " |>> ignore)
                    <|> spaces1
                    <|> (parseComment |>> ignore)
                )

            do! skipChar openChar
            do! sep

            let! items = many (parseForm .>> sep)

            do! skipChar closeChar

            return
                List.filter
                    (function
                    | Skip -> false
                    | _ -> true)
                    items
        }

    let private parseList = parseBetween '(' ')' |>> List

    let private parseVector =
        parseBetween '[' ']'
        |>> (fun l -> Vector(Array.ofList l))

    parseFormRef
    := choice [ parseComment
                parseNil
                parseFloat
                parseBool
                parseString
                parseList
                parseVector
                parseKeyword
                parseQuote
                parseQuasiQuote
                parseSpliceUnquote
                parseUnquote
                parseDeref
                parseSymbol ]

    let readString str =
        match run !parseFormRef str with
        | Success (result, _, _) -> Result.Ok result
        | Failure (err, _, _) -> Result.Error err
