namespace FsMal.Test

open FsMal.Types
open Xunit
open Xunit.Abstractions

type TypesTest(output: ITestOutputHelper) =
    [<Fact>]
    let ``equality for Nil`` () =
        Assert.Equal(Nil, Nil)
        Assert.NotEqual(Nil, Skip)

    [<Fact>]
    let ``equality for Skip`` () =
        Assert.Equal(Skip, Skip)
        Assert.NotEqual(Skip, Nil)

    [<Fact>]
    let ``equality for String`` () =
        Assert.Equal(String "abc", String "abc")
        Assert.NotEqual(String "abc", Nil)

    [<Fact>]
    let ``equality for Number`` () =
        Assert.Equal(Number 1.0, Number 1.0)
        Assert.NotEqual(Number 1.0, Nil)

    [<Fact>]
    let ``equality for Bool`` () =
        Assert.Equal(Bool true, Bool true)
        Assert.NotEqual(Bool true, Bool false)
        Assert.NotEqual(Bool true, Nil)

    [<Fact>]
    let ``equality for List`` () =
        Assert.Equal(
            List [ Nil;String "abc";Number 1.0 ],
            List [ Nil;String "abc";Number 1.0 ]
        )

        Assert.NotEqual(List [ Nil ], List [ Number 1.0 ])
        Assert.NotEqual(List [ Nil ], Nil)

    [<Fact>]
    let ``equality for Vector`` () =
        Assert.Equal(
            Vector [| Nil;String "abc";Number 1.0 |],
            Vector [| Nil;String "abc";Number 1.0 |]
        )

        Assert.NotEqual(Vector [| Nil |], Vector [| Number 1.0 |])
        Assert.NotEqual(Vector [| Nil |], Nil)

    [<Fact>]
    let ``equality for Symbol`` () =
        Assert.Equal(Symbol "abc", Symbol "abc")
        Assert.NotEqual(Symbol "abc", Nil)

    [<Fact>]
    let ``equality for Keyword`` () =
        Assert.Equal(Keyword "abc", Keyword "abc")
        Assert.NotEqual(Keyword "abc", Nil)

    [<Fact>]
    let ``equality for Atom`` () =
        Assert.Equal(Atom(ref <| Number 1.0), Atom(ref <| Number 1.0))
        Assert.NotEqual(Atom(ref <| Number 1.0), Nil)

    [<Fact>]
    let ``equality for HashMap`` () =
        Assert.Equal(
            HashMap(Map.empty, Map.ofSeq [ "a", Nil ]),
            HashMap(Map.empty, Map.ofSeq [ "a", Nil ])
        )

        Assert.NotEqual(
            HashMap(Map.empty, Map.ofSeq [ "a", Nil ]),
            HashMap(Map.ofSeq [ "b", Number 1.0 ], Map.empty)
        )

        Assert.NotEqual(HashMap(Map.empty, Map.ofSeq [ "a", Nil ]), Nil)

    [<Fact>]
    let ``equality for BuiltInFunc`` () =
        Assert.Equal(
            BuiltInFunc(1, (fun _ -> Nil)),
            BuiltInFunc(1, (fun _ -> Nil))
        )

        Assert.NotEqual(
            BuiltInFunc(1, (fun _ -> Nil)),
            BuiltInFunc(2, (fun _ -> Nil))
        )

    [<Fact>]
    let ``equality for non Node object`` () = Assert.False(Nil.Equals(123))

    [<Fact>]
    let ``comparision of Node`` () =
        let set =
            Set.ofList [ Nil
                         Skip
                         Number 1.0
                         String "abc"
                         Bool true
                         List [ Nil ]
                         Vector [| Nil |]
                         Symbol "bcd"
                         Keyword "cde"
                         Atom <| ref Nil
                         HashMap(Map.empty, Map.empty)
                         BuiltInFunc(1, (fun _ -> Nil)) ]

        let set' =
            Set.ofList [ Nil
                         Skip
                         Number 1.0
                         String "abc"
                         Bool true
                         List [ Nil ]
                         Vector [| Nil |]
                         Symbol "bcd"
                         Keyword "cde"
                         Atom <| ref Nil
                         HashMap(Map.empty, Map.empty)
                         BuiltInFunc(1, (fun _ -> Nil)) ]

        Assert.True(set.Equals(set'))
        Assert.False(Nil.Equals(1))

    [<Fact>]
    let ``hashcode of Node`` () =
        let hashes =
            [ Nil
              Skip
              Number 1.0
              String "abc"
              Bool true
              List [ Nil ]
              Vector [| Nil |]
              Symbol "bcd"
              Keyword "cde"
              Atom <| ref Nil
              HashMap(Map.empty, Map.empty)
              BuiltInFunc(1, (fun _ -> Nil)) ]
            |> List.map (fun item -> item.GetHashCode())

        let hashes' =
            [ Nil
              Skip
              Number 1.0
              String "abc"
              Bool true
              List [ Nil ]
              Vector [| Nil |]
              Symbol "bcd"
              Keyword "cde"
              Atom <| ref Nil
              HashMap(Map.empty, Map.empty)
              BuiltInFunc(1, (fun _ -> Nil)) ]
            |> List.map (fun item -> item.GetHashCode())

        Assert.True(hashes.Equals(hashes'))
