module Day03

open Puzzles.Day03
open Expecto
open Samples

[<Tests>]
let puzzle1Tests =
    testList "Puzzle 1" [
        testCase "Sample of good triangle" <| fun _ ->
            let sampleInput = "5 25 25"
            Expect.equal (countTriangles [|sampleInput|]) 1 sampleInput
        testCase "Sample of bad triangle" <| fun _ ->
            let sampleInput = "5 10 25"
            Expect.equal (countTriangles [|sampleInput|]) 0 sampleInput
        testCase "Puzzle question 1" <| fun _->
            let puzzleInput = readAllLines "Day03.input.txt"
            Expect.equal (countTriangles puzzleInput) 1050 "Puzzle question 1"
    ] 
