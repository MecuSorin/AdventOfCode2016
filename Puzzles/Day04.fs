module Puzzles.Day04

open System
open System.Text.RegularExpressions
let regexPattern = new Regex ("^(?<name>[a-z\-]*)(?<code>\d+)\[(?<checksum>[a-z]{5})\]$")

type Room = { Name: string; Code: int; Checksum: string}
let getRoom input =
    let matches = regexPattern.Match(input)
    if matches.Success 
    then
        Some { 
                Name = matches.Groups.["name"].Value
                Code = matches.Groups.["code"].Value |> int
                Checksum = matches.Groups.["checksum"].Value
             }
    else
        None

let computeChecksum room = 
    room.Name.Replace("-", String.Empty)
    |> Seq.groupBy id
    |> Seq.sortByDescending (fun (f, s) -> 1000 * (Seq.length s) - int f)
    |> Seq.take 5
    |> Seq.map fst
    |> String.Concat

let getRealRoom roomOption =
    match roomOption with
    | None -> None
    | Some room ->
        if room.Checksum = computeChecksum room 
        then roomOption
        else None

let getCodeFromRealRoom roomOption =
    match getRealRoom roomOption with
    | Some r -> r.Code
    | None -> 0
        
let getSumOfRealRooms inputLines =
    inputLines
    |> Seq.sumBy (getRoom >> getCodeFromRealRoom)

let azDelta = int 'z' - int 'a' + 1
let shiftToRight positions ch =
    match ch with
    | '-' -> if 0 = positions % 2 then '-' else ' '
    | ' ' -> if 0 = positions % 2 then ' ' else '_'
    | _ ->  char ((int ch - int 'a' + positions ) % azDelta + int 'a')

let decryptRoom room =
    room 
    |> Option.map (fun r-> 
                        let decodedName =
                            r.Name
                            |> Seq.map (shiftToRight r.Code)
                            |> String.Concat
                        decodedName, r.Code)
