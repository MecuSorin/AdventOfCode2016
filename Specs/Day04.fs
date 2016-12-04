module Day04

open Puzzles.Day04
open Expecto
open Samples

let helperTestCase testNamePrefix input expected (evaluator: string -> int) =
    testCase (testNamePrefix + input) <| fun _ ->
        Expect.equal (evaluator input) expected (testNamePrefix + input)

let goodRoomTestCase input expected = helperTestCase "Good room " input expected (getRoom >> getCodeFromRealRoom)

let badRoomTestCase input = helperTestCase "Bad room " input 0 (getRoom >> getCodeFromRealRoom)

let sampleRoom = {Name = "aaaaa-bbb-z-y-x-"; Code = 123; Checksum = "abxyz"}

[<Tests>]
let puzzle1Tests =
    testList "Day 04 Puzzle 1" [
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

[<Tests>]
let puzzle2Tests =
    testList "Day 04 Puzzle 2" [
        testCase "Decrypt room name " <| fun _ ->
            let input = Some { Name = "qzmt-zixmtkozy-ivhz-"; Code = 343; Checksum = ""}
            Expect.equal (decryptRoom input) (Some ("very encrypted name ", 343)) "Decrypt room name"
        testCase "Day 04 Puzzle question 2" <| fun _->
            let puzzleInput = readAllLines "Day04.input.txt"
            let firstRoomWithNorthPole =
                puzzleInput
                |> Array.map (getRoom >> getRealRoom >> decryptRoom)
                // |> Array.iter ( Option.iter (fun (n,c) -> printfn "%i\t%s" c n))
                |> Array.filter Option.isSome
                |> Array.map Option.get
                |> Array.filter (fun (n, _) -> n.Contains "northpole")
                |> Array.head
            Expect.equal (snd firstRoomWithNorthPole) 501 "Day 04 Puzzle question 2"
    ]