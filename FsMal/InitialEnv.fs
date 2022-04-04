namespace FsMal

module InitialEnv =
    open Error
    open Types

    let typeMismatchError expected args =
        EvalException $"Expected type %s{expected}, got %s{typeToString args}"

    let addFun =
        function
        | [ Number a; Number b ] -> Number(a + b)
        | args -> raise (typeMismatchError "(number number)" (List args))

    let subFun =
        function
        | [ Number a; Number b ] -> Number(a - b)
        | args -> raise (typeMismatchError "(number number)" (List args))

    let makeEnv () =
        let env = Env.makeEmpty ()
        let _ = Env.set env "+" (BuiltInFunc(1, addFun))
        let _ = Env.set env "-" (BuiltInFunc(2, subFun))

        env
