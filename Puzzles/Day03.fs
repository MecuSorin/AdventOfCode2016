module Puzzles.Day03

open System

let isTriangle (triangleLengths: int[]) =
    triangleLengths.[0] < triangleLengths.[1] + triangleLengths.[2]   // assume a sane input
    && triangleLengths.[1] < triangleLengths.[0] + triangleLengths.[2] 
    && triangleLengths.[2] < triangleLengths.[1] + triangleLengths.[0]
    
let haveSameColumn triangleLengths =
    let asStringsLengths = triangleLengths |> Array.map string
    let smallest = 
        asStringsLengths
        |> Array.map String.length
        |> Array.min
    [1 .. smallest]
    |> List.map (fun digitIndex ->
                    asStringsLengths
                    |> Array.map (fun lengthAsString -> lengthAsString.[ lengthAsString.Length - digitIndex])
                    |> Array.distinct
                    |> Array.length)
    |> List.exists ((=) 1 )
    

let getLengths (input: String[]) =
    input
    |> Array.map (fun row -> 
                    row.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map int)

let countTriangles input =
    getLengths input
    |> Array.filter isTriangle
    |> Array.length

let countTrianglesVertically input =
    let lengths = getLengths input
    [0..2]
    |> List.map (fun x ->
        lengths
        |> Array.map (fun row -> row.[x])
        |> Array.chunkBySize 3
        |> Array.filter isTriangle 
        // |> Array.filter haveSameColumn   // missreaded the instructions :(
        |> Array.length)
    |> List.sum
