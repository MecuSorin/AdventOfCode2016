namespace Tests

module ``Day 01`` =
    open Xunit
    open FsUnit.Xunit


    [<Fact>]
    let ``Expected project reference to work`` () =
        Puzzles.Puzzles().X |> should equal "F##"