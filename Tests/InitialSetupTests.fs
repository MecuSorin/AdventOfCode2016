namespace Tests

module ``Before Starting`` =
    open Xunit
    open FsUnit.Xunit


    [<Fact>]
    let ``Check that the project reference is working`` () =
        Puzzles.``Initial setup``.Loving().X |> should equal "F#"

