open Expecto

module Main =
    [<Tests>]
    let tests =
        testCase "F# love <| yes" <| fun () ->
            let expected = InitialSetup.Loving().X
            Expect.equal expected "F#"
                    "The strings should equal"

[<EntryPoint>]
let main args =
  runTestsInAssembly defaultConfig args
