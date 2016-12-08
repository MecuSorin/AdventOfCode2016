module Day06

open Puzzles.Day06
open Expecto
open Samples
open System
let samplePuzzle1 = toLines """eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar"""


[<Tests>]
let puzzle1Tests =
    testList "Day 06 Puzzle 1" [
         simpleEqualTestCase "Sample 1" (lazy(getPredominant samplePuzzle1)) "easter"
         

         test "Puzzle 1" {
             let lines = readAllLines "Day06.input.txt"
             Expect.equal (getPredominant lines) "qzedlxso" "Puzzle 1"
         }
    ]
