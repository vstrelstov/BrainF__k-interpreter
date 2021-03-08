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
        Memory: uint array;
    }

let maxOperationsCount = int <| 1e5

[<EntryPoint>]
let main argv =
    let linesNumber = (Console.ReadLine() |> (fun s -> s.Split [|' '|]) |> Array.map (int)).[1]
    let input = Console.ReadLine() |> (fun s -> s.TrimEnd('$')) |> Seq.toList

    let rec getCommands currentLineNumber commands = 
        let alphabet = [|'>'; '<'; '+'; '-'; ','; '.'; '['; ']'|]

        let getCommand = function
            | '<' -> Command.Previous
            | '>' -> Command.Next
            | '+' -> Command.Inc
            | '-' -> Command.Dec
            | '.' -> Command.Write
            | ',' -> Command.Read
            | '[' -> Command.LoopStart
            | ']' -> Command.LoopEnd

        if currentLineNumber > linesNumber then
            commands
        else
            let newCommands = Console.ReadLine() |> Seq.toArray |> Array.filter (fun c -> Array.contains c alphabet) |> Array.map (fun c -> getCommand c)
            getCommands (currentLineNumber + 1) (Array.append commands newCommands)

    // TODO: Consider using list instead of array. Keep in mind that memory length can be infinite 
    let program = { Commands = (getCommands 1 [||]); Memory = Array.zeroCreate maxOperationsCount; }

    let execute program =
        let rec executionLoop opCount commandPointer dataPointer input =
            printfn "%i" opCount
            if opCount > maxOperationsCount then 
                Console.WriteLine("\nPROCESS TIME OUT. KILLED!!!")    
            elif commandPointer < (Array.length program.Commands) then
                let setDataPointer value = executionLoop (opCount + 1) (commandPointer + 1) (dataPointer + value) input
                
                let modifyMemoryCell increment = 
                    program.Memory.[dataPointer] <- if increment then program.Memory.[dataPointer] + 1u else program.Memory.[dataPointer] - 1u
                    executionLoop (opCount + 1) (commandPointer + 1) dataPointer input

                let read () =
                    let tryTail list = 
                        match list with
                        | [] -> []
                        | _::tail -> tail
                    program.Memory.[dataPointer] <- (List.tryHead input).Value |> uint
                    executionLoop (opCount + 1) (commandPointer + 1) dataPointer (tryTail input)

                let write () =
                    Console.Write(program.Memory.[dataPointer] |> char)
                    executionLoop (opCount + 1) (commandPointer + 1) dataPointer input

                let jumpToLoopBound decNestedCommand incNestedCommand moveCmdPtrFunc =
                    let rec jumpToLoopBoundRec currentCmdPtr nestedCount =
                        let currentCommand = program.Commands.[currentCmdPtr]
                        let jmp = currentCmdPtr + moveCmdPtrFunc
                        if currentCommand = decNestedCommand && nestedCount = 1 then 
                            currentCmdPtr
                        else match currentCommand with 
                            | x when x = decNestedCommand -> jumpToLoopBoundRec jmp (nestedCount - 1)
                            | y when y = incNestedCommand -> jumpToLoopBoundRec jmp (nestedCount + 1)
                            | _ -> jumpToLoopBoundRec jmp nestedCount

                    jumpToLoopBoundRec commandPointer 0

                let jmpToLoopStart() = 
                    let newCommandPointer = jumpToLoopBound Command.LoopStart Command.LoopEnd  -1
                    executionLoop (opCount + 1) newCommandPointer dataPointer input
                let jmpToLoopEnd() = 
                    let newCommandPointer = jumpToLoopBound Command.LoopEnd Command.LoopStart 1
                    executionLoop (opCount + 1) newCommandPointer dataPointer input

                match program.Commands.[commandPointer] with
                | Command.Inc -> modifyMemoryCell true
                | Command.Dec -> modifyMemoryCell false
                | Command.Previous -> setDataPointer -1
                | Command.Next -> setDataPointer 1
                | Command.Read -> read ()
                | Command.Write -> write ()
                | Command.LoopStart ->
                    if program.Memory.[dataPointer] = 0u then jmpToLoopEnd () else executionLoop (opCount + 1) (commandPointer + 1) dataPointer input
                | Command.LoopEnd ->
                    if program.Memory.[dataPointer] <> 0u then jmpToLoopStart () else executionLoop (opCount + 1) (commandPointer + 1) dataPointer input

        executionLoop 0 0 0 input

    execute program

    0
