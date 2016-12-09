module Day08

open Puzzles.Day08
open Expecto
open Samples
open System

let parserTest input expected =
    test input {
        Expect.equal (parseMatrixCommand input) (Some expected) input
    }

[<Tests>]
let puzzle1Tests =
    testList "Day 08 Puzzle 1" [
        testList "Parsers tests" [
            parserTest "rotate column x=10 by 3" (RotateColumn (10, 3))
            parserTest "rect 13x3" (Rect (13, 3))
            parserTest "rotate row y=5 by 15" (RotateRow (5, 15))
        ]

        testCase "Shifting array" <| fun _ ->
            Expect.equal (shiftArray 1 [|on; on; off|]) [|off; on; on|] "Shifting array"
        
        testCase "Sample provided" <| fun _ ->
            let initialMatrix = createMatrix 3 7
            let afterMatrix = 
                initialMatrix
                |> rect 3 2
                |> rotateColumn 1 1
                |> rotateRow 0 4
                |> rotateColumn 1 1
            let expectedMatrix = [|
                [| off; on; off; off; on; off; on |]
                [| on; off; on; off; off; off; off |]
                [| off; on; off; off; off; off; off |]
            |]
            Expect.equal (computeLightsCount afterMatrix)  6 "counted lights"
            Expect.equal afterMatrix expectedMatrix "checked matrixes"

        test "Puzzle 1 input" {
            let lines = readAllLines "Day08.input.txt"
            Expect.equal (evaluateVolatage 6 50 lines) 106 "Puzzle 1 input" 
        }
    ]
