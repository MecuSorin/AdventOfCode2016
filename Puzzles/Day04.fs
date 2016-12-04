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
                Name = matches.Groups.["name"].Value.Replace("-", String.Empty)
                Code = matches.Groups.["code"].Value |> int
                Checksum = matches.Groups.["checksum"].Value
             }
    else
        None

let computeChecksum room = 
    room.Name
    |> Seq.groupBy id
    |> Seq.sortByDescending (fun (f, s) -> 1000 * (Seq.length s) - int f)
    |> Seq.take 5
    |> Seq.map fst
    |> String.Concat

let getCodeFromRealRoom roomOption =
    match roomOption with
    | None -> 0
    | Some room ->
        if room.Checksum = computeChecksum room 
        then room.Code 
        else 0
        
let getSumOfRealRooms inputLines =
    inputLines
    |> Seq.sumBy (getRoom >> getCodeFromRealRoom)
