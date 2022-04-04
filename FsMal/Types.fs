namespace FsMal

open System

module Types =
    // TODO: add support for user functions, macros
    [<CustomEquality; CustomComparison>]
    type Form =
        | Nil
        | Number of number: float
        | String of str: string
        | Bool of bool: bool
        | List of list: Form list
        | Vector of vec: Form array
        | Symbol of sym: string
        | Keyword of keyword: string
        | Atom of ref: Form ref
        | HashMap of
            keywordMap: Map<string, Form> *
            stringMap: Map<string, Form>
        | BuiltInFunc of tag: int * implementation: (Form list -> Form)
        | Skip
        // Skip is a special type which is used to mark the parts of code which
        // is are evaluated. It can not be used from the LISP userspace

        interface IComparable with
            member this.CompareTo other =
                match other with
                | :? Form as other' -> (this :> IComparable<_>).CompareTo other'
                | _ -> -1

        interface IComparable<Form> with
            member this.CompareTo other =
                match this, other with
                | Skip, Skip
                | Nil, Nil -> 0
                | Number a, Number b -> compare a b
                | String a, String b -> compare a b
                | Bool a, Bool b -> compare a b
                | List a, List b -> compare a b
                | Vector a, Vector b -> compare a b
                | Symbol a, Symbol b -> compare a b
                | Keyword a, Keyword b -> compare a b
                | Atom a, Atom b -> compare a b
                | HashMap (a, a'), HashMap (b, b') -> compare (a, a') (b, b')
                | BuiltInFunc (a, _), BuiltInFunc (b, _) -> compare a b
                | _, _ -> -1

        override this.Equals other =
            match other with
            | :? Form as otherForm ->
                match this, otherForm with
                | Skip, Skip
                | Nil, Nil -> true
                | Number a, Number b -> a = b
                | String a, String b -> a = b
                | Bool a, Bool b -> a = b
                | List a, List b -> a = b
                | Vector a, Vector b -> a = b
                | Symbol a, Symbol b -> a = b
                | Keyword a, Keyword b -> a = b
                | Atom a, Atom b -> a = b
                | HashMap (a, a'), HashMap (b, b') -> a = b && a' = b'
                | BuiltInFunc (a, _), BuiltInFunc (b, _) -> a = b
                | _, _ -> false
            | _ -> false

        override this.GetHashCode() =
            match this with
            | Skip
            | Nil -> 0
            | Number a -> hash a
            | String a -> hash a
            | Bool a -> hash a
            | List a -> hash a
            | Vector a -> hash a
            | Symbol a -> hash a
            | Keyword a -> hash a
            | Atom a -> hash a
            | HashMap (a, a') -> hash (a, a')
            | BuiltInFunc (a, _) -> hash $"bif<%d{a}>"

    let private concatSpace = String.concat " "

    let rec typeToString =
        function
        | Skip -> "skip"
        | Nil -> "nil"
        | Number _ -> "number"
        | String _ -> "string"
        | Bool _ -> "bool"
        | List l -> $"(%s{l |> List.map typeToString |> concatSpace})"
        | Vector v ->
            $"[%s{v
                  |> Array.map typeToString
                  |> Array.toList
                  |> concatSpace}]"
        | Symbol s -> s
        | Keyword k -> $":%s{k}"
        | Atom a -> $"(atom $s{typeToString a.Value})"
        | HashMap _ -> "hashmap"
        | BuiltInFunc _ -> "#bif<>"
