module Samples 

open System.IO

[<Literal>]
let SampleInputFolder = "Specs/SampleInputs/"
let file fileName =
    System.IO.Path.Combine [|SampleInputFolder; fileName|]

let readAllLines fileName =
    File.ReadAllLines (file fileName)

type STestsAttribute() = inherit System.Attribute()