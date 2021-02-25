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

[<EntryPoint>]
let main argv =
    let linesNumber = (Console.ReadLine() |> (fun s -> s.Split [|' '|]) |> Array.map (int)).[1]
    let input = Console.ReadLine() |> (fun s -> s.TrimEnd('$')) |> Seq.toList

    let rec getTokens currentLineNumber tokens = 
        let alphabet = [|'>'; '<'; '+'; '-'; ','; ','; '['; ']'|]

        let getToken = function
            | '<' -> Token.Previous
            | '>' -> Token.Next
            | '+' -> Token.Inc
            | '-' -> Token.Dec
            | '.' -> Token.Write
            | ',' -> Token.Read
            | '[' -> Token.LoopStart
            | ']' -> Token.LoopEnd

        match currentLineNumber with
        | linesNumber -> tokens
        | _ -> 
            let newTokens = Console.ReadLine() |> Seq.toList |> List.filter (fun c -> Array.contains c alphabet) |> List.map (fun c -> getToken c)
            getTokens (currentLineNumber + 1) (tokens @ newTokens)

    let tokens = getTokens 1 []
    let (memory: byte[]) = Array.zeroCreate maxOperationsCount


    0
