module Puzzles.Day09

open System.Text
open FParsec

// AST
type DecompresserStep =
    | Uncompressed of string
    | Repeatable of repetitions: int * toRepeat: string 
    | WhiteSpace 
let decompressSteps decopressorStepsList =
    let sb = new StringBuilder()
    decopressorStepsList
    |> Seq.iter (fun ds ->
        match ds with
        | Uncompressed txt -> sb.Append(txt) |> ignore
        | Repeatable (steps, txt) -> 
            for i = 1 to steps do sb.Append(txt) |> ignore
        | WhiteSpace -> ())
    sb.ToString()

// parsers

type UserState = unit 
type Parser<'t> = Parser<'t, UserState>


let parserGrabber :Parser<_> = 
    many1Satisfy (fun c-> c <> '(' && c<> ' ' && c<> '\t' && c<> '\n' && c <> '\r') |>> Uncompressed
let parserMarker = skipStringCI "(" >>. pint32 .>> skipStringCI "x" .>>. pint32 .>> skipStringCI ")"

let buildParserRepeat (noOfChars, repetitions) = 
    tuple2 (preturn repetitions) (anyString noOfChars)

let parserMarkerComposed = parserMarker >>= buildParserRepeat |>> Repeatable
let whiteSpaces = spaces1 |>> (fun _ -> WhiteSpace)
let parserDecompresser = many (choice [parserMarkerComposed; parserGrabber; whiteSpaces]) .>> eof

let parseDecompressorSteps (text:string) =
    match run parserDecompresser text with
    | Success(result, _, _) -> Some result
    | Failure(errorMsg, _, _) -> None

let decompress input =
    parseDecompressorSteps input
    |> Option.map decompressSteps