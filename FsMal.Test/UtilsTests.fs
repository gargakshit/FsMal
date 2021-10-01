namespace FsMal.Test

open FsMal.Utils
open System.Collections.Generic
open Xunit
open Xunit.Abstractions

type UtilsTests(output: ITestOutputHelper) =
    [<Fact>]
    let ``GetOption from GenericDictExtensions`` () =
        let dict = Dictionary()
        dict.["key"] <- true

        Assert.Equal(dict.GetOption "key", Some true)
        Assert.Equal(dict.GetOption "non_existent", None)

    [<Fact>]
    let ``OptionBuilder`` () =
        Assert.Equal(option { return! Some true }, Some true)

        Assert.Equal(
            option {
                let! _ = None
                return! Some true
            },
            None
        )
        
        Assert.Equal(
            option {
                let! _ = None
                return "success"
            },
            None
        )
        
        Assert.Equal(
            option {
                let! _ = Some true
                return "success"
            },
            Some "success"
        )
