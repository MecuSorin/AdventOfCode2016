module Day05

open Puzzles.Day05
open Expecto
open Samples

[<Tests>]
let puzzle1Tests =
    testList "Day 05 Puzzle 1" [
        simpleEqualTestCase "Hash of abc3231929 should start with 000001" 
            (lazy(md5 "abc3231929")) "00000155f8105dff7f56ee10fa9b9abd"
        simpleEqualTestCase "Finding first password char for abc" 
            (lazy(unfolder "abc" 3231929)) (Some ((Some ("5", "1")), 3231930))
        simpleEqualTestCase "Getting the first password char for abc" 
            (lazy(unfolder "abc" 3231928)) (Some (None, 3231929)) 
    ]

[<STests>]
let puzzle1LongerTests =
    testList "Day 05 Puzzle 1 with beard" [
        simpleEqualTestCase "Sample abc"
            (lazy(getPassword "abc" 0)) "18f47a30" 
        simpleEqualTestCase "Puzzle question 1 single thread"
            (lazy(getPassword "uqwqemis" 0)) "1a3099aa"
        simpleEqualTestCase "Puzzle question 1 in parallel" 
            (lazy(parallelGetPassword "uqwqemis" 0)) "1a3099aa"
    ] 

[<STests>]
let puzzle2Tests =
    testList "Day 05 Puzzle 2 with beard" [
        simpleEqualTestCase  "Sample abc"  
            (lazy(parallelGetAdvancedPassword "abc" 3231928)) "05ace8e3"
             
        simpleEqualTestCase "Puzzle question 2 single thread"
            (lazy(getAdvancedPassword "uqwqemis" 0)) "?"
        
        simpleEqualTestCase "Puzzle question 2 in parallel" 
            (lazy (parallelGetAdvancedPassword "uqwqemis" 0)) "694190cd"
    ]