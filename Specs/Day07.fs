module Day07

open Puzzles.Day07
open Expecto
open Samples

let isTLSTest input expected = 
    test input {
        Expect.equal (isTLS input) expected input
    }

[<Tests>]
let puzzle1Tests =
    testList "Day 07 Puzzle 1" [
        test "tls" {
            Expect.isTrue (tls.IsMatch("abba")) "abba"
            Expect.isFalse (tls.IsMatch("aaaa")) "aaaa"
        }

        isTLSTest "abba[mnop]qrst" true
        isTLSTest "abcd[bddb]xyyx" false
        isTLSTest "aaaa[qwer]tyui" false 
        isTLSTest "ioxxoj[asdfgh]zxcvbn" true 
        
        test "Puzzle 1" {
             let counted = 
                readAllLines "Day07.input.txt"
                |> Seq.filter isTLS
                |> Seq.length
             Expect.equal counted 110 "Puzzle 1"
        }
    ]

