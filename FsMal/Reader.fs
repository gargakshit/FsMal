namespace FsMal

module Reader =
    open FParsec
    open Types
    open Utils

    let private quote = Symbol "quote"
    let private quasiquote = Symbol "quasiquote"
    let private unquote = Symbol "unquote"
    let private spliceUnquote = Symbol "splice-unquote"
    let private deref = Symbol "deref"

    let private parseComment =
        skipChar ';' >>. restOfLine false >>% Skip

    let private parseForm, private parseFormRef: Parser<Form, unit> * Parser<Form, unit> ref =
        createParserForwardedToRef ()

    let private parseNil = pstring "nil" >>% Nil

    let private parseFloat = pfloat |>> Number

    let private parseBool =
        (stringReturn "true" (Bool true))
        <|> (stringReturn "false" (Bool false))

    let private allowedChars =
        noneOf "\n\r \"(),;[\\]{}"
        <?> "alphanumeric characters"

    let private parseSymbol = many1Chars allowedChars |>> Symbol

    let private parseKeyword =
        skipChar ':' <?> "keyword"
        >>. many1Chars allowedChars
        |>> Keyword

    let private parsePrefixed prefix = skipString prefix >>. parseForm

    let private parseSpecial prefix symbol =
        parsePrefixed prefix
        |>> (fun form -> List [ symbol; form ])

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

        betweenL
            (pstring "\"")
            (pstring "\"")
            (stringsSepBy normalCharSnippet escapedChar)
            "string literal in double quotes"
        |>> String

    let private sep =
        skipMany (
            (anyOf ",\n " |>> ignore)
            <|> spaces1
            <|> (parseComment |>> ignore)
        )
        <?> "separator"

    let private filterSkip =
        function
        | Skip -> false
        | _ -> true

    let private parseBetween openChar closeChar =
        skipChar openChar
        >>. sep
        >>. many (parseForm .>> sep)
        .>> skipChar closeChar
        |>> List.filter filterSkip

    let private parseList = parseBetween '(' ')' |>> List

    let private parseVector =
        parseBetween '[' ']' |>> (Array.ofList >> Vector)

    let private parseHashMap =
        skipChar '{'
        >>. sep
        >>. many (
            parseKeyword <|> parseString .>> sep
            .>>. parseForm
            .>> sep
        )
        .>> skipChar '}'
        |>> List.fold
                (fun (keywordMap, stringMap) ->
                    function
                    | String str, form ->
                        (keywordMap, Map.add str form stringMap)
                    | Keyword str, form ->
                        (Map.add str form keywordMap, stringMap)
                    // Return original maps in case if the key is not a string
                    // or a keyword. Due to how the parser works, this situation
                    // is not possible
                    | _ -> (keywordMap, stringMap))
                (Map.empty, Map.empty)
        |>> HashMap

    parseFormRef.Value <-
        choiceL
            [ parseComment
              parseNil
              parseFloat
              parseBool
              parseString
              parseList
              parseVector
              parseKeyword
              parseHashMap
              parseQuote
              parseQuasiQuote
              parseSpliceUnquote
              parseUnquote
              parseDeref
              parseSymbol ]
            "FsMal form"

    let readString (str: string) =
        match str.Trim() with
        | "" -> Result.Ok Skip
        | str ->
            match run parseFormRef.Value str with
            | Success (result, _, _) -> Result.Ok result
            | Failure (err, _, _) -> Result.Error err
