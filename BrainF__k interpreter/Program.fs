open System

type Command = 
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

    let rec getCommands currentLineNumber tokens = 
        let alphabet = [|'>'; '<'; '+'; '-'; ','; ','; '['; ']'|]

        let getCommand = function
            | '<' -> Command.Previous
            | '>' -> Command.Next
            | '+' -> Command.Inc
            | '-' -> Command.Dec
            | '.' -> Command.Write
            | ',' -> Command.Read
            | '[' -> Command.LoopStart
            | ']' -> Command.LoopEnd

        match currentLineNumber with
        | linesNumber -> tokens
        | _ -> 
            let newTokens = Console.ReadLine() |> Seq.toArray |> Array.filter (fun c -> Array.contains c alphabet) |> Array.map (fun c -> getCommand c)
            getCommands (currentLineNumber + 1) (Array.append tokens newTokens)

    let tokens = getCommands 1 [||]
    // TODO: Probably use list instead of array. Keep in mind that memory length can be infinite
    let (memory: byte[]) = Array.zeroCreate maxOperationsCount 
    
    let interpret tokens = 
        let tryTail list = 
            match list with
            | [] -> []
            | _::tail -> tail

        // TODO: Consider using of currying or partial application
        // TODO: Add loops support
        // TODO: Add operations count
        let rec interpreterLoop tokens dataPointer input output =
            match tokens with
            | [] -> output
            | head::tail ->
                let partial = interpreterLoop tail
                match head with
                | Command.Next -> partial (dataPointer + 1) input output
                | Command.Previous -> partial (dataPointer - 1) input output
                | Command.Inc -> 
                    memory.[dataPointer] <- (memory.[dataPointer] + 1uy)
                    partial dataPointer input output
                | Command.Dec -> 
                    memory.[dataPointer] <- (memory.[dataPointer] - 1uy)
                    partial dataPointer input output
                | Command.Read -> 
                    memory.[dataPointer] <- (List.head input |> byte)
                    partial dataPointer (tryTail input) output
                | Command.Write -> partial dataPointer input (Array.append output [|(memory.[dataPointer] |> char)|])

        interpreterLoop tokens 0 input [||]

    0
