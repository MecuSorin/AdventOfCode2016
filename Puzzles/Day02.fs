module Puzzles.Day02

open System

type KeypadMove =
    | Up
    | Down
    | Left
    | Right

let moveOnKeypad key instruction =
    match instruction, key with
    | Up, k when k<4 -> k
    | Up, _ -> key - 3
    | Down, k when k>6 -> k
    | Down, _ -> key + 3
    | Left, k when 1 = k % 3 -> k
    | Left, _ -> key - 1
    | Right, k when 0 = k % 3 -> k
    | Right, _ -> key + 1 

let moveOnAdvancedKeyPad key instruction =
    match instruction, key with
    | Up, 3 -> 1
    | Up, 13 -> 11
    | Up, k when k>5 && k <> 9 -> k - 4
    | Up, k -> k
    | Down, 11 -> 13
    | Down, 1 -> 3
    | Down, k when k < 9 && k <>5 -> k + 4
    | Down, k -> k
    | Left, 1 -> 1
    | Left, 2 -> 2
    | Left, 5 -> 5
    | Left, 10 -> 10
    | Left, 13 -> 13
    | Left, k -> k - 1
    | Right, 1 -> 1
    | Right, 4 -> 4
    | Right, 9 -> 9
    | Right, 12 -> 12
    | Right, 13 -> 13
    | Right, k -> k + 1
    

let getKeyPadMove letter =
    match letter with
    | 'U' -> Some Up
    | 'D' -> Some Down
    | 'L' -> Some Left
    | 'R' -> Some Right
    | _ -> None


let genericCodeFinder moveOnKeyPadStrategy (input: String) (startAt: int) =
    let listOfRows = Array.toList (input.Split ([|'\r'; '\n'|], System.StringSplitOptions.RemoveEmptyEntries))
    (startAt, listOfRows)
    |> Seq.unfold (fun (startingKey, rows) ->
            match rows with
            | [] -> None
            | row :: tail ->
                row
                |> Seq.map getKeyPadMove
                |> Seq.filter Option.isSome
                |> Seq.map Option.get
                |> Seq.fold moveOnKeyPadStrategy startingKey
                |> fun resultedKey -> Some (resultedKey, (resultedKey, tail)))
    |> Seq.map (sprintf "%X")
    |> String.concat ""

let findTheCodeSimpleKeypad input =
    genericCodeFinder moveOnKeypad input 5

let findTheCodeAdvancedKeypad input =
    genericCodeFinder moveOnAdvancedKeyPad input 5