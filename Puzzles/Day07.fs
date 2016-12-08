module Puzzles.Day07

open System
open System.Text.RegularExpressions

let tlsPattern = "(\w{1})(?!\1)(\w{1})\2\1"
let tls = new Regex(tlsPattern)
let antiTls = new Regex("\[\w*" + tlsPattern + "\w*\]")
let isTLS input = 
    (not (antiTls.IsMatch(input))) && (tls.IsMatch(input))