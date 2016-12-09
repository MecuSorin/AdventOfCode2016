module Puzzles.Day07

open System
open System.Text.RegularExpressions

let tlsPattern = "(\w{1})(?!\1)(\w{1})\2\1"
let tls = new Regex(tlsPattern)
let antiTls = new Regex("\[\w*" + tlsPattern + "\w*\]")
let isTLSRegex input = 
    (not (antiTls.IsMatch(input))) && (tls.IsMatch(input))

// puzzle 2 without regex
let isABAMatch (listOf3Letters: char []) = 
    listOf3Letters.[0] <> listOf3Letters.[1] && listOf3Letters.[0] = listOf3Letters.[2]

let listOfABA (text : string) = 
    text
    |> Seq.windowed 3
    |> Seq.filter isABAMatch
    |> Seq.map (String.Concat)

let partitions (row: string) =
    let comutatingFolder (odd, even) chunk = 
        chunk :: even, odd
    row.Split ([|'[';']'|])
    |> Array.fold comutatingFolder ([],[])

let containReflection (listOfABAItems: string list) (aba: string) =
    let reflection =
        String.Concat [ aba.[1]; aba.[0]; aba.[1]]
    listOfABAItems
    |> List.exists (fun txt -> txt.Contains(reflection))

let isSSL row =
    let hyperNet, superNet = partitions row
    superNet
    |> Seq.collect listOfABA
    |> Seq.exists (containReflection hyperNet)


// puzzle 2 regex attempt, not used anymore since is taking to long
let abaPattern = "(\w{1})(?!\1)(\w{1})\1"
let babPattern = "\2\1\2"

let anyPattern = "(?:\w*\[\w*\]\w*)*"     // check that [ have pair ]
let sslPattern1 = anyPattern + abaPattern + anyPattern + "\[\w*" + babPattern + "\w*\]"
let sslPattern2 = anyPattern + "\[\w*" + abaPattern + "\w*\]" + anyPattern + babPattern

let sslRegex1 = new Regex( sslPattern1)
let sslRegex2 = new Regex ( sslPattern2)

let isSSLRegex input = 
    sslRegex1.IsMatch(input) || sslRegex2.IsMatch(input)

