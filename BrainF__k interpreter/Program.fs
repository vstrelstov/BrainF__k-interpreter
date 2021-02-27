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

type BfProgram =
    {
        Commands: Command array;
        Memory: byte array;
    }

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
            let newCommands = Console.ReadLine() |> Seq.toArray |> Array.filter (fun c -> Array.contains c alphabet) |> Array.map (fun c -> getCommand c)
            getCommands (currentLineNumber + 1) (Array.append tokens newCommands)

    // TODO: Consider using list instead of array. Keep in mind that memory length can be infinite 
    let program = { Commands = (getCommands 1 [||]); Memory = Array.zeroCreate maxOperationsCount; }

    let execute program =
        let rec executionLoop opCount commandPointer dataPointer input = 
            let nextIteration = executionLoop (opCount + 1)
            let nextCommandBase = nextIteration (commandPointer + 1)
            let nextCommand = nextCommandBase dataPointer

            if opCount > maxOperationsCount then 
                Console.WriteLine("\nPROCESS TIME OUT. KILLED!!!")    
            elif commandPointer < ((Array.length program.Commands) - 1) then
                let setDataPointer value = nextCommandBase (dataPointer + value) input
                
                let modifyMemoryCell modifyOperation = 
                    program.Memory.[dataPointer] <- program.Memory.[dataPointer] |> modifyOperation
                    nextCommand input

                let read = 
                    let tryTail list = 
                        match list with
                        | [] -> []
                        | _::tail -> tail
                    program.Memory.[dataPointer] <- (List.tryHead input).Value |> byte
                    nextCommand (tryTail input)

                let write = 
                    Console.Write(program.Memory.[dataPointer] |> char)
                    nextCommand input

                match program.Commands.[commandPointer] with
                | Command.Inc -> modifyMemoryCell ((+)1uy)
                | Command.Dec -> modifyMemoryCell ((-)1uy)
                | Command.Previous -> setDataPointer -1
                | Command.Next -> setDataPointer 1
                | Command.Read -> read
                | Command.Write -> write

        executionLoop 0 0 0 input

    execute program

    0
