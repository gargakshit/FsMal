namespace FsMal

module Printer =
    open Types

    let rec printToString pretty =
        function
        | Nil -> "nil"
        | Number num -> num.ToString()
        | String str -> printString pretty str
        | Bool false -> "false"
        | Bool true -> "true"
        | Keyword k -> $":%s{k}"
        | Symbol s -> s
        | Atom v -> $"(atom %s{printToString pretty v.Value})"
        | Skip -> ""
        | List l -> $"(%s{printList pretty l})"
        | Vector v -> $"[%s{printArray pretty v}]"
        | BuiltInFunc _ -> "#<function>"
        | HashMap (keywordMap, stringMap) ->
            printHashMap pretty keywordMap stringMap

    and private conditionalSpaceJoin str str' =
        match str with
        | "" -> str'
        | _ -> str + " " + str'

    and private printMap keyword pretty =
        let keyForm = if keyword then Keyword else String

        Map.toList
        >> List.fold
            (fun acc (k, v) ->
                conditionalSpaceJoin
                    acc
                    $"%s{printToString pretty (keyForm k)} %s{printToString pretty v}")
            ""

    and private printHashMap pretty keywordMap stringMap =
        sprintf
            "{%s%s%s}"
            (printMap true pretty keywordMap)
            (if (Map.isEmpty stringMap) then
                 ""
             else
                 " ")
            (printMap false pretty stringMap)

    and private printList pretty =
        List.map (printToString pretty)
        >> List.fold conditionalSpaceJoin ""

    and private printArray pretty =
        Array.map (printToString pretty)
        >> Array.fold conditionalSpaceJoin ""

    and private printString pretty = if pretty then prettyPrintStr else id

    and private prettyPrintStr str =
        (str
         |> Seq.map
             (function
             | '\n' -> "\\n"
             | '\t' -> "\\t"
             | '\r' -> "\\r"
             | '\"' -> "\\\""
             | '\\' -> "\\\\"
             | c -> $"%c{c}")
         |> Seq.fold (sprintf "%s%s") "\"")
        + "\""
