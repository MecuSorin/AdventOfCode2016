module Puzzles.Day03

open System

let isTriangle lengths =
    let sortedLengths = lengths |> Array.sortDescending
    sortedLengths.[0] < sortedLengths.[1] + sortedLengths.[2]   // assume a sane input

let getLengths (input: String[]) =
    input
    |> Array.map (fun row -> 
                    row.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map int)

let countTriangles input =
    getLengths input
    |> Array.filter isTriangle
    |> Array.length