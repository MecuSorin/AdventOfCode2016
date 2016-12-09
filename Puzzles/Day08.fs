module Puzzles.Day08

open System
open FParsec

let on = "#"
let off = "."

let createMatrix rows cols = 
    Array.init rows (fun _ -> Array.init cols (fun _ -> off))
let rect cols rows matrix =
    matrix
    |> Array.mapi (fun rindex row ->
        if rows <= rindex
        then row
        else
            row |> Array.skip cols
            |> Array.append (Array.init cols (fun _ -> on)))

let shiftArray by items =
    let lngth = Array.length items
    items
    |> Array.take (lngth - by)
    |> Array.append (items |>  Array.skip (lngth - by))

let rotateRow ri by matrix =
    matrix
    |> Array.mapi (fun rindex row ->
        if ri <> rindex
        then row
        else
            row |> shiftArray by)

let rotateColumn ci by (matrix:'T[][]) =
    let byProjections = 
        let current =
            matrix 
            |> Array.map (fun row -> row.[ci])
        shiftArray by current
    matrix
    |> Array.mapi (fun rIndex row ->
        row
        |> Array.mapi (fun cIndex col ->
            if cIndex = ci then byProjections.[rIndex] else col)) 

let computeLightsCount matrix =
    matrix
    |> Array.concat
    |> Array.filter ((=) on)
    |> Array.length

// AST ================================================================================
type MatrixCommand =
    | Rect of cols:int * rows: int
    | RotateRow of row:int * by: int
    | RotateColumn of col:int * by: int

let runCommand matrix = function
    | Rect (c, r) -> rect c r matrix
    | RotateRow (r, b) -> rotateRow r b matrix
    | RotateColumn (c, b) -> rotateColumn c b matrix

let rec runCommands matrix commands  =
    commands
    |> Seq.fold runCommand matrix

// parsers =============================================================================
type UserState = unit 
type Parser<'t> = Parser<'t, UserState>

let parserInt: Parser<_> = spaces >>. pint32 .>> spaces
let parserStr str = spaces >>. skipStringCI str .>> spaces
let parserRect = parserStr "rect" >>. parserInt .>> parserStr "x" .>>. parserInt |>> Rect
let parserRotateRow = parserStr "rotate row y=" >>. parserInt .>> parserStr "by" .>>. parserInt |>> RotateRow
let parserRotateColumn = parserStr "rotate column x=" >>. parserInt .>> parserStr "by" .>>. parserInt |>> RotateColumn
let parserMatrixCommand = choice [parserRect; parserRotateRow; parserRotateColumn] .>> eof
let parseMatrixCommand (text:string) =
    match run parserMatrixCommand text with
    | Success(result, _, _) -> Some result
    | Failure(errorMsg, _, _) -> None

let evaluateMatrix rows cols commands =
    commands
    |> Array.choose parseMatrixCommand
    |> runCommands (createMatrix rows cols)

