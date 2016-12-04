module Day04

open Puzzles.Day04
open Expecto
open Samples

let helperTestCase testNamePrefix input expected (evaluator: string -> int) =
    testCase (testNamePrefix + input) <| fun _ ->
        Expect.equal (evaluator input) expected (testNamePrefix + input)

let goodRoomTestCase input expected = helperTestCase "Good room " input expected (getRoom >> getCodeFromRealRoom)

let badRoomTestCase input = helperTestCase "Bad room " input 0 (getRoom >> getCodeFromRealRoom)

let sampleRoom = {Name = "aaaaabbbzyx"; Code = 123; Checksum = "abxyz"}

[<Tests>]
let puzzle1Tests =
    testList "Puzzle 1" [
        testCase "Identify room info" <| fun _ ->
            let actual = getRoom "aaaaa-bbb-z-y-x-123[abxyz]"        
            let expected = Some sampleRoom
            Expect.equal actual expected "Identify room info"

        testCase "Compute room checksum" <| fun _ ->
            Expect.equal (computeChecksum sampleRoom) "abxyz" "Compute checksum"
        
        testCase "Get code from a good room" <| fun _ ->
            let actual = getCodeFromRealRoom (Some sampleRoom)
            Expect.equal actual 123 "Get code from a good room"

        goodRoomTestCase "aaaaa-bbb-z-y-x-123[abxyz]" 123
        goodRoomTestCase "a-b-c-d-e-f-g-h-987[abcde]" 987
        goodRoomTestCase "not-a-real-room-404[oarel]" 404
        badRoomTestCase "totally-real-room-200[decoy]"

        testCase "Puzzle question 1" <| fun _->
            let puzzleInput = readAllLines "Day04.input.txt"
            Expect.equal (getSumOfRealRooms puzzleInput) 137896 "Puzzle question 1"
    ] 
