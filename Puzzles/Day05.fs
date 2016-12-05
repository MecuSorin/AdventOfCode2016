module Puzzles.Day05

open System.Security.Cryptography
open System.Text

let passwordSize = 8
let md5 (text: string) =
    let data = System.Text.Encoding.ASCII.GetBytes(text)
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string


let getNextChar (roomId: string) (currentIndex: int) =
    let hash = md5 (roomId + (string currentIndex))
    if isNull hash || 7 > hash.Length
    then failwith "Failed to create the md5 hash"
    elif hash.StartsWith("00000")
    then 
        // printfn "Found a number: %s ------------------- from index: %i" hash currentIndex
        Some (hash.Substring (6, 1), hash.Substring (5,1))
    else None

let unfolder roomId nextIndex =
    Some (getNextChar roomId nextIndex, nextIndex + 1)


let between0AndPasswordSize  (_, place) = "0" <= place && place <= string (passwordSize - 1 )
let ignoreFilter _ = true
let sequentialGetPasswordRaw roomId startIndex filterResults =
    Seq.unfold (unfolder roomId) startIndex
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> Seq.filter filterResults
    |> Seq.take passwordSize
  
let getPassword roomId startIndex =
    sequentialGetPasswordRaw roomId startIndex ignoreFilter
    |> Seq.map fst
    |> String.concat ""

let getAdvancedPassword roomId startIndex =
    sequentialGetPasswordRaw roomId startIndex between0AndPasswordSize
    |> Seq.sortBy snd
    |> Seq.map fst
    |> String.concat ""
    
let parallelGetPassword roomId startIndex =
    if System.Environment.ProcessorCount = 1
    then getPassword roomId startIndex
    else  
        let computeNext withIndex = withIndex, getNextChar roomId withIndex
        
        let getNextBatch currentIndex =
            let nextIndex = currentIndex + System.Environment.ProcessorCount * 1000
            let results = 
                Async.Parallel [ for i in currentIndex .. nextIndex -> async { return computeNext i }]
                |> Async.RunSynchronously
                |> Array.filter (snd >> Option.isSome)
                |> Array.map ( fun (x, o) -> x, Option.get o)
            results, nextIndex + 1

        let rec loop someIndex (sofar: (int * (string * string)) []) =
            if sofar.Length >= passwordSize
            then
                sofar
                |> Array.sortBy fst
                |> Array.take passwordSize
                |> Array.map (snd >> fst)
                |> String.concat ""
            else
                let nextBatchResults, newIndex = getNextBatch someIndex
                let untilNow = sofar |> Array.append nextBatchResults
                loop newIndex untilNow
        loop startIndex [||]

type KeyCode = { fromIndex:int; position: string; key: string }

type KeyCode
    with static member Create (x, (y, z)) = { fromIndex = x; position = z; key = y }
    

// created another function because adapting first to common specs would make it harder to read
let parallelGetAdvancedPassword roomId startIndex =
    if System.Environment.ProcessorCount = 1
    then 
        // printfn "Single thread ..."
        getAdvancedPassword roomId startIndex
    else  
        // printfn "Parallel on %i cores" System.Environment.ProcessorCount
        let computeNext withIndex = withIndex, getNextChar roomId withIndex
        
        let getNextBatch currentIndex =
            let nextIndex = currentIndex + System.Environment.ProcessorCount * 50000
            let results = 
                Async.Parallel [ for i in currentIndex .. nextIndex -> async { return computeNext i }]
                |> Async.RunSynchronously
                |> Array.filter (snd >> Option.isSome)
                |> Array.map ( fun (x, o) -> x, Option.get o)
                |> Array.filter (snd >> between0AndPasswordSize)
                |> Array.map (KeyCode.Create)
            results, nextIndex + 1

        let rec loop someIndex (sofar: KeyCode []) =
            let positions =
                sofar
                |> Array.map (fun kc -> kc.position)
                |> Array.distinct
                |> Array.length
            
            if positions >= passwordSize
            then
                sofar
                |> Array.groupBy (fun kc -> kc.position)
                |> Array.sortBy fst
                |> Array.take passwordSize
                |> Array.map (fun (k, kcGroup) -> 
                    kcGroup
                    |> Seq.sortBy (fun kc -> kc.fromIndex)
                    |> Seq.head
                    |> fun kc-> kc.key)
                |> String.concat ""
            else
                // printfn "Parallel at: %i" someIndex
                let nextBatchResults, newIndex = getNextBatch someIndex
                let untilNow = sofar |> Array.append nextBatchResults
                loop newIndex untilNow
        loop startIndex [||]