open System

type Token = 
    | Next
    | Previous
    | Inc
    | Dec
    | Write
    | Read
    | LoopStart
    | LoopEnd

let maxOperationsCount = int <| 1e5

let readToken = 
    let symbol = char <| Console.Read()
    match symbol with
    | '<' -> Token.Previous
    | '>' -> Token.Next
    | '+' -> Token.Inc
    | '-' -> Token.Dec
    | '.' -> Token.Write
    | ',' -> Token.Read
    | '[' -> Token.LoopStart
    | ']' -> Token.LoopEnd

[<EntryPoint>]
let main argv =
    
    0
