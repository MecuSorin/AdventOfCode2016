module Samples 

open System
open System.IO
open Expecto

[<Literal>]
let SampleInputFolder = "Specs/SampleInputs/"
let file fileName =
    System.IO.Path.Combine [|SampleInputFolder; fileName|]

let readAllLines fileName =
    File.ReadAllLines (file fileName)

let toLines (txt: string) = txt.Split([|"\n"|], StringSplitOptions.RemoveEmptyEntries)

type STestsAttribute() = inherit System.Attribute()

let simpleEqualTestCase<'T when 'T: equality> name (actual: Lazy<'T>) (expected:'T) =
    testCase name <| fun _ ->
        Expect.equal (actual.Force()) expected name