module Puzzles.Day06

open System


let rawListProcessing sorter items =
    items
    |> List.groupBy id
    |> sorter (snd >> List.length)
    |> List.head
    |> fst

let mostCommon = rawListProcessing (List.sortByDescending)
let scarce = rawListProcessing (List.sortBy)

let getByDistribution (distributionMapper: char list -> char) (input: string []) =
    let verticalSplices = Array.init ((Array.head input).Length) (fun _ -> [])
     
    input
    |> Array.fold (fun (cumulative: char list []) row ->
        row
        |> Seq.mapi (fun i letter -> letter :: (cumulative.[i]))
        |> Seq.toArray) verticalSplices
    |> Array.map distributionMapper
    |> String.Concat    

let getPredominant = getByDistribution mostCommon

let getScarce = getByDistribution scarce