module Day01

open Puzzles.Day01
open Expecto
open Samples

let puzzleQuestion = """L1, L3, L5, L3, R1, L4, L5, R1, R3, L5, R1, L3, L2, L3, R2, R2, L3, L3, R1, L2, R1, L3, L2, R4, R2, L5, R4, L5, R4, L2, R3, L2, R4, R1, L5, L4, R1, L2, R3, R1, R2, L4, R1, L2, R3, L2, L3, R5, L192, R4, L5, R4, L1, R4, L4, R2, L5, R45, L2, L5, R4, R5, L3, R5, R77, R2, R5, L5, R1, R4, L4, L4, R2, L4, L1, R191, R1, L1, L2, L2, L4, L3, R1, L3, R1, R5, R3, L1, L4, L2, L3, L1, L1, R5, L4, R1, L3, R1, L2, R1, R4, R5, L4, L2, R4, R5, L1, L2, R3, L4, R2, R2, R3, L2, L3, L5, R3, R1, L4, L3, R4, R2, R2, R2, R1, L4, R4, R1, R2, R1, L2, L2, R4, L1, L2, R3, L3, L5, L4, R4, L3, L1, L5, L3, L5, R5, L5, L4, L2, R1, L2, L4, L2, L4, L1, R4, R4, R5, R1, L4, R2, L4, L2, L4, R2, L4, L1, L2, R1, R4, R3, R2, R2, R5, L1, L2"""

let day01TestCase input expected (evaluator: string -> int) =
    testCase input <| fun _ ->
        Expect.equal (evaluator input) expected input

let puzzle1TestCase input expected = day01TestCase input expected evaluateBunnyHeadquatersDistance
let puzzle2TestCase input expected = day01TestCase input expected evaluateBunnyHeadquatersDistance2
    
[<Tests>]
let puzzle1Tests =
    testList "Day 01 Puzzle 1" [
        puzzle1TestCase "R2, L3" 5
        puzzle1TestCase "R2, R2, R2" 2
        puzzle1TestCase "R5, L5, R5, R3" 12
        simpleEqualTestCase "Puzzle question 1" 
            (lazy(evaluateBunnyHeadquatersDistance puzzleQuestion)) 299
    ] 

[<Tests>]
let puzzle2Tests =
    testList "Day 01 Puzzle 2" [
        puzzle2TestCase "R8, R4, R4, R8" 4
        simpleEqualTestCase "Puzzle question 2" (lazy(evaluateBunnyHeadquatersDistance2 puzzleQuestion)) 181
    ]