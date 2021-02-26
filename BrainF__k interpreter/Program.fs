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
    // TODO: Probably use list instead of array. Keep in mind that memory length can be infinite
    let (memory: byte[]) = Array.zeroCreate maxOperationsCount 
    
    let interpret tokens = 
        let tryTail list = 
            match list with
            | [] -> []
            | _::tail -> tail

        // TODO: Consider using of currying or partial application
        // TODO: Add loops support
        let rec interpreterLoop tokens dataPointer input output =
            match tokens with
            | [] -> output
            | head::tail ->
                let partial = interpreterLoop tail
                match head with
                | Token.Next -> partial (dataPointer + 1) input output
                | Token.Previous -> partial (dataPointer - 1) input output
                | Token.Inc -> 
                    memory.[dataPointer] <- (memory.[dataPointer] + 1uy)
                    partial dataPointer input output
                | Token.Dec -> 
                    memory.[dataPointer] <- (memory.[dataPointer] - 1uy)
                    partial dataPointer input output
                | Token.Read -> 
                    memory.[dataPointer] <- (List.head input |> byte)
                    partial dataPointer (tryTail input) output
                | Token.Write -> partial dataPointer input (Array.append output [|(memory.[dataPointer] |> char)|])

        interpreterLoop tokens 0 input [||]

    let output = interpret tokens |> Array.map (fun c -> c.ToString()) |> String.concat ""
    Console.WriteLine(output)

    0
