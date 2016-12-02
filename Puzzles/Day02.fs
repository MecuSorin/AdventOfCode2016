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

let getKeyPadMove letter =
    match letter with
    | 'U' -> Some Up
    | 'D' -> Some Down
    | 'L' -> Some Left
    | 'R' -> Some Right
    | _ -> None


let findTheCode (input: String) (startAt: int) =
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
                |> Seq.fold moveOnKeypad startingKey
                |> fun resultedKey -> Some (resultedKey, (resultedKey, tail)))
    |> Seq.map string
    |> String.concat ""