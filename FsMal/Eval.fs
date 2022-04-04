namespace FsMal

module Eval =
    open Env
    open Error
    open Types

    let rec evalAst env =
        function
        | Symbol s -> get env s
        | List l -> List(List.map (eval env) l)
        | Vector v -> Vector(Array.map (eval env) v)
        | ast -> ast

    and eval env form =
        match form with
        | List []
        | Vector [||] -> form
        | List l ->
            match List.map (evalAst env) l with
            | [] -> List [] // Impossible case
            | f :: args ->
                match f with
                | BuiltInFunc (_, f) -> f args
                | other ->
                    raise (
                        EvalException
                            $"Type %s{typeToString other} is not a function"
                    )
        | form -> evalAst env form
