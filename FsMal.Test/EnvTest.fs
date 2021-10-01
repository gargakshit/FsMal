namespace FsMal.Test

open FsMal.Env
open FsMal.Error
open FsMal.Utils
open FsMal.Types
open Xunit
open Xunit.Abstractions

type EnvTest(output: ITestOutputHelper) =
    [<Fact>]
    let ``makeEmpty creates an empty env`` () =
        let env = makeEmpty ()

        Assert.Equal(env.data.GetOption "something", None)

    [<Fact>]
    let ``set adds data to the env`` () =
        let env = makeEmpty ()
        let _ = set env "key" <| String "value"

        Assert.Equal(env.data.GetOption "key", Some <| String "value")

    [<Fact>]
    let ``fork returns a new env with old env as parent`` () =
        let env = makeEmpty ()
        let env' = fork env

        Assert.Equal(env'.parent, Some env)

    [<Fact>]
    let ``find finds the correct env containing the key`` () =
        let env = makeEmpty ()
        let _ = set env "key" <| String "value"
        let env' = fork env

        Assert.Equal(find env' "key", Some env)

        let env = makeEmpty ()
        let env' = fork env
        let _ = set env' "key" <| String "value"

        Assert.Equal(find env' "key", Some env')

        let env = makeEmpty ()
        let _ = set env "key" <| String "value"
        let env' = fork env
        let env'' = fork env'

        Assert.Equal(find env'' "key", Some env)

        let env = makeEmpty ()

        Assert.Equal(find env "key", None)

    [<Fact>]
    let ``tryGet returns the data associated with the key`` () =
        let env = makeEmpty ()
        let _ = set env "key" <| String "value"
        let env' = fork env

        Assert.Equal(tryGet env' "key", Some <| String "value")

        let env = makeEmpty ()

        Assert.Equal(tryGet env "key", None)

    [<Fact>]
    let ``get raises an exception when no associated data is found`` () =
        let env = makeEmpty ()
        let _ = set env "key" <| String "value"
        let env' = fork env

        Assert.Equal(get env' "key", String "value")

        let env = makeEmpty ()

        Assert.Throws<EvalException>(fun () -> get env "key" |> ignore)
        |> ignore
