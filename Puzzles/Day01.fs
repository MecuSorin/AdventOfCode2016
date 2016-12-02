module Puzzles.Day01

open System

type Direction =
    | North
    | South
    | East
    | West

type MoveInstruction =
    | Left of int
    | Right of int

type Position = { NorthPositive: int; WestPositive: int }

type State = { Position: Position; Direction: Direction }

let goWest pos value = { pos with WestPositive = pos.WestPositive + value }
let goEast pos value = goWest pos (- value)
let goNorth pos value = { pos with NorthPositive = pos.NorthPositive + value }
let goSouth pos value = goNorth pos (- value)

let move currentState moveInstruction =
    match currentState.Direction, moveInstruction with
    | North, Left a
    | South, Right a -> { Direction = West; Position = goWest currentState.Position a }
    | North, Right a
    | South, Left a -> { Direction = East; Position = goEast currentState.Position a }
    | East, Left a
    | West, Right a -> { Direction = North; Position = goNorth currentState.Position a }
    | East, Right a
    | West, Left a -> { Direction = South; Position = goSouth currentState.Position a }


let computeDistanceFromStart position =
    Math.Abs (position.NorthPositive) + Math.Abs (position.WestPositive)

let parseInt str =
    let mutable result = 0
    if Int32.TryParse(str, &result) then Some result else None

let getMoveCommand (str: String) =
    match str |> Seq.toList with
    | 'L' :: _ -> str.Substring(1) |> parseInt |> Option.map Left
    | 'R' :: _ -> str.Substring(1) |> parseInt |> Option.map Right
    | _ -> None

let getInstructionsList (instructions:String) =
    instructions.Split ([|' '; ','|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map getMoveCommand
    |> Array.filter Option.isSome
    |> Array.map Option.get

let evaluateBunnyHeadquatersDistance instructions =
    getInstructionsList instructions
    |> Array.fold move { Position = { NorthPositive = 0; WestPositive = 0}; Direction = North }
    |> fun s-> s.Position
    |> computeDistanceFromStart

//////////////////////////////////////////////////////////////////////////////////////////
// Day 01 puzzle part 2

type Track = Set<Position>

type StateWithHistory = { 
    Position: Position 
    Heading: Direction
    Track: Track
    AlreadyVisited: Position option }

let visit (track:Track) locations =
    let rec findExisting locs =
        match locs with
        | [] -> None
        | head::tail -> 
            if track.Contains head 
            then Some head
            else findExisting tail
    findExisting locations,  track + (new Track( locations))

let generateLocations pos toIndex goStrategy=
    [1 .. toIndex] 
    |> List.map (fun x -> goStrategy pos x)
let goWestLocations pos toX = generateLocations pos toX goWest
let goEastLocations pos toX = generateLocations pos toX goEast
let goNorthLocations pos toY = generateLocations pos toY goNorth
let goSouthLocations pos toY = generateLocations pos toY goSouth


let moveWithHistory current moveInstruction =
    match current.Heading, moveInstruction with
    | North, Left a 
    | South, Right a -> 
        let visited, track = visit current.Track (goWestLocations current.Position a)
        {
        Heading = West
        Position = goWest current.Position a 
        Track = track
        AlreadyVisited = visited
        }
    | North, Right a
    | South, Left a -> 
        let visited, track = visit current.Track (goEastLocations current.Position a)
        {
        Heading = East
        Position = goEast current.Position a 
        Track = track
        AlreadyVisited = visited
        }
    | East, Left a
    | West, Right a -> 
        let visited, track = visit current.Track (goNorthLocations current.Position a)
        {
        Heading = North
        Position = goNorth current.Position a 
        Track = track
        AlreadyVisited = visited
        }
    | East, Right a
    | West, Left a -> 
        let visited, track = visit current.Track (goSouthLocations current.Position a)
        {
        Heading = South
        Position = goSouth current.Position a 
        Track = track
        AlreadyVisited = visited
        }
    
let evaluateBunnyHeadquatersDistance2 instructions =
    let initialState = { 
                        Position = { NorthPositive = 0; WestPositive = 0}
                        Heading = North
                        AlreadyVisited = None
                        Track = Set.empty }
    let rec loopUntilFound state listOfCommands =
        match listOfCommands with
        | [] -> state.Position
        | head::tail ->
            let newState = moveWithHistory state head
            match newState.AlreadyVisited with
            | Some pos -> pos
            | None -> loopUntilFound newState tail

    getInstructionsList instructions
    |> Array.toList
    |> loopUntilFound initialState 
    |> computeDistanceFromStart