module Day07

open Puzzles.Day07
open Expecto
open Samples
open System

let isTLSRegexTest input expected = 
    test input {
        Expect.equal (isTLSRegex input) expected input
    }

let isSSLRegexTest input expected = 
    test input {
        Expect.equal (isSSLRegex input) expected input
    }

let isSSLTest input expected = 
    test input {
        Expect.equal (isSSL input) expected input
    }

[<Tests>]
let puzzle1Tests =
    testList "Day 07 Puzzle 1" [
        test "tls" {
            Expect.isTrue (tls.IsMatch("abba")) "abba"
            Expect.isFalse (tls.IsMatch("aaaa")) "aaaa"
        }

        isTLSRegexTest "abba[mnop]qrst" true
        isTLSRegexTest "abcd[bddb]xyyx" false
        isTLSRegexTest "aaaa[qwer]tyui" false 
        isTLSRegexTest "ioxxoj[asdfgh]zxcvbn" true 
        
        test "Puzzle 1" {
             let counted = 
                readAllLines "Day07.input.txt"
                |> Seq.filter isTLSRegex
                |> Seq.length
             Expect.equal counted 110 "Puzzle 1"
        }
    ]

[<Tests>]
let puzzle2RegexTests =
    testList "Day 07 Puzzle 2/regex" [

        isSSLRegexTest "aba[bab]xyz" true
        isSSLRegexTest "xyx[xyx]xyx" false
        isSSLRegexTest "zazbz[bzb]cdb" true 
        isSSLRegexTest "aaa[kek]eke" true 
    ]

let puzzle2RegexPuzzle = 
    // is taking too long and have a regex bug (to few matches) => no more regex
    test "Puzzle 2 with regex" {
        readAllLines "Day07.input.txt"
        |> Array.mapi (fun i v -> async { 
            printfn "%i %s" i v
            return isSSL v })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.filter id
        |> Seq.length |> ignore
    }

[<Tests>]
let puzzle2WithoutRegex =
    testList "Day 07 Puzzle 2/no regex" [
        isSSLTest "aba[bab]xyz" true
        isSSLTest "xyx[xyx]xyx" false
        isSSLTest "zazbz[bzb]cdb" true 
        isSSLTest "aaa[kek]eke" true

        test "Puzzle 2 input" {
            let counted = 
                readAllLines "Day07.input.txt"
                |> Seq.filter isSSL
                |> Seq.length
            Expect.equal counted 242 ""
        }
    ]
