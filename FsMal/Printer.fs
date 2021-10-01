namespace FsMal

module Printer =
    open Types

    let rec printString pretty =
        function
        | Nil -> "nil"
        | Number num -> num.ToString()
        | String str when pretty -> prettyPrintStr str
        | String str -> str
        | Bool false -> "false"
        | Bool true -> "true"
        | Keyword k -> $":%s{k}"
        | Symbol s -> s
        | Atom v -> $"(atom %s{printString pretty !v})"
        | Skip -> ""
        | List l -> $"(%s{printList pretty l})"
        | Vector v -> $"[%s{printArray pretty v}]"

    and private conditionalSpaceJoin str str' =
        match str with
        | "" -> str'
        | _ -> str + " " + str'

    and private printList pretty =
        List.map (printString pretty)
        >> List.fold conditionalSpaceJoin ""

    and private printArray pretty =
        Array.map (printString pretty)
        >> Array.fold conditionalSpaceJoin ""

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
