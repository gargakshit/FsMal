namespace FsMal

module InitialEnv =
    open Types

    let typeMismatchError expected args =
        ErrorForm $"Expected type %s{expected}, got %s{typeToString args}"

    let addFun =
        function
        | [ Number a; Number b ] -> Number(a + b)
        | args -> typeMismatchError "(number number)" (List args)

    let subFun =
        function
        | [ Number a; Number b ] -> Number(a - b)
        | args -> typeMismatchError "(number number)" (List args)

    let makeEnv () =
        let env = Env.makeEmpty ()
        let _ = Env.set env "+" (BuiltInFunc(1, addFun))
        let _ = Env.set env "-" (BuiltInFunc(2, subFun))

        env
