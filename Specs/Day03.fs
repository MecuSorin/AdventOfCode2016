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

[<Tests>]
let puzzle2Tests =
    testList "Puzzle 2" [
        testCase "Same column cases" <| fun _ ->
            Expect.isTrue (haveSameColumn [|102;205;601|]) "Same column"
            Expect.isTrue (haveSameColumn [|102;2;62|]) "Same column"
            Expect.isTrue (haveSameColumn [|202;205;210|]) "Same column"
            Expect.isTrue (haveSameColumn [|202;205;203|]) "Same column"

            Expect.isFalse (haveSameColumn [|12;23;34|]) "No identical column"
            Expect.isFalse (haveSameColumn [|123;12;1|]) "No identical column"
            Expect.isFalse (haveSameColumn [|123;22;12|]) "No identical column"
            Expect.isFalse (haveSameColumn [|23;12;2|]) "No identical column"
            
        testCase "Sample of good triangle" <| fun _ ->
            let sampleInput = [|" 5   5 25"
                                "10  24 20"
                                "11  24 22"
                                "13   4 25"
                                " 1 104  5"
                                " 0   1 22"|]
            Expect.equal (countTrianglesVertically sampleInput) 4 "Cheching vertically for triangles sample"

        testCase "Puzzle question 2" <| fun _->
            let puzzleInput = readAllLines "Day03.input.txt"
            let actual = countTrianglesVertically puzzleInput
            Expect.equal actual 1921 "Puzzle question 2"
    ] 

