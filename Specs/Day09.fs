module Day09

open Puzzles.Day09
open Expecto
open Samples
open System

let decompressTest input expected =
    testCase ("Decompressing " + input) <| fun _ ->
        Expect.equal (decompress input) (Some expected) ("Decompressing " + input)

[<Tests>]
let puzzle1Tests =
    testList "Day 09 Puzzle 1" [
        decompressTest "ADVENT" "ADVENT"
        decompressTest "A(1x5)BC" "ABBBBBC"
        decompressTest "(3x3)XYZ" "XYZXYZXYZ"
        decompressTest "A(2x2)BCD(2x2)EFG" "ABCBCDEFEFG"
        decompressTest "(6x1)(1x3)A" "(1x3)A"
        decompressTest "X(8x2)(3x3)ABCY" "X(3x3)ABC(3x3)ABCY"
        testCase "Puzzle 1 input" <| fun _ ->
            let size = 
                readAllText "Day09.input.txt"
                |> decompress
                |> Option.get
                |> String.length
            Expect.equal size 70186 "Checking length of puzzle 1" 
    ]