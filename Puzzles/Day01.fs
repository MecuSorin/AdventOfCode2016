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

let move currentState moveInstruction =
    match currentState.Direction, moveInstruction with
    | North, Left a
    | South, Right a -> {
                        Direction = West
                        Position = { currentState.Position with WestPositive = currentState.Position.WestPositive + a }}
    | North, Right a
    | South, Left a -> {
                        Direction = East
                        Position = { currentState.Position with WestPositive = currentState.Position.WestPositive - a }}
    | East, Left a
    | West, Right a -> {
                        Direction = North
                        Position = { currentState.Position with NorthPositive = currentState.Position.NorthPositive + a}}
    | East, Right a
    | West, Left a -> {
                        Direction = South
                        Position = { currentState.Position with NorthPositive = currentState.Position.NorthPositive - a}}

let computeDistanceFromStart state =
    Math.Abs (state.Position.NorthPositive) + Math.Abs (state.Position.WestPositive)

let parseInt str =
    let mutable result = 0
    if Int32.TryParse(str, &result) then Some result else None

let getMoveCommand (str: String) =
    match str |> Seq.toList with
    | 'L' :: _ -> str.Substring(1) |> parseInt |> Option.map Left
    | 'R' :: _ -> str.Substring(1) |> parseInt |> Option.map Right
    | _ -> None

let evaluateBunnyHeadquatersDistance (instructions:String) =
    instructions.Split ([|' '; ','|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map getMoveCommand
    |> Array.filter Option.isSome
    |> Array.map Option.get
    |> Array.fold move { Position = { NorthPositive = 0; WestPositive = 0}; Direction = North }
    |> computeDistanceFromStart
