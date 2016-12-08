module Puzzles.Day06

open System

let mostCommon items =
    items
    |> List.groupBy id
    |> List.sortByDescending (snd >> List.length)
    |> List.head
    |> fst

let getPredominant (input: string []) =
    let verticalSplices = Array.init ((Array.head input).Length) (fun _ -> [])
     
    input
    |> Array.fold (fun (cumulative: char list []) row ->
        row
        |> Seq.mapi (fun i letter -> letter :: (cumulative.[i]))
        |> Seq.toArray) verticalSplices
    |> Array.map mostCommon
    |> String.Concat    