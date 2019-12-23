module AOC2019.Day2

let program = [|1;12;2;3;1;1;2;3;1;3;4;3;1;5;0;3;2;13;1;19;1;10;19;23;1;23;9;27;1;5;27;31;2;31;13;35;1;35;5;39;1;39;5;43;2;13;43;47;2;47;10;51;1;51;6;55;2;55;9;59;1;59;5;63;1;63;13;67;2;67;6;71;1;71;5;75;1;75;5;79;1;79;9;83;1;10;83;87;1;87;10;91;1;91;9;95;1;10;95;99;1;10;99;103;2;103;10;107;1;107;9;111;2;6;111;115;1;5;115;119;2;119;13;123;1;6;123;127;2;9;127;131;1;131;5;135;1;135;13;139;1;139;10;143;1;2;143;147;1;147;10;0;99;2;0;14;0|]

type Add = {
    LeftPosition : int
    RightPosition : int
    OutputPosition : int
}

type Multiply = {
    LeftPosition : int
    RightPosition : int
    OutputPosition : int
}

type Instruction =
    | Add of Add
    | Multiply of Multiply
    | Halt
    | Unknown

    
let getInstructions (program : int[]) =
    let rec getInstruction ip =
        seq {
            match program.[ip] with
            | 1 ->
                yield Add { LeftPosition = program.[ip + 1]; RightPosition = program.[ip + 2]; OutputPosition = program.[ip + 3] }
                yield! getInstruction (ip + 4)
            | 2 ->
                yield Multiply { LeftPosition = program.[ip + 1]; RightPosition = program.[ip + 2]; OutputPosition = program.[ip + 3] }
                yield! getInstruction (ip + 4)
            | 99 ->
                yield Halt
            | _ ->
                yield Unknown
        }
        
    getInstruction 0

let patchProgram =
    program.[1] = 12 |> ignore
    program.[2] = 2 |> ignore
    
let run (program : int[]) =
    let mutable pc = 0
    
    while program.[pc] <> 99 do
        match program.[pc] with
        | 1 ->
            let opr1Pos = program.[pc + 1]
            let opr2Pos = program.[pc + 2]
            let opr3Pos = program.[pc + 3]
            program.[opr3Pos] <- program.[opr1Pos] + program.[opr2Pos]
            pc <- pc + 4
        | 2 ->
            let opr1Pos = program.[pc + 1]
            let opr2Pos = program.[pc + 2]
            let opr3Pos = program.[pc + 3]
            program.[opr3Pos] <- program.[opr1Pos] * program.[opr2Pos]
            pc <- pc + 4
        | _ -> failwith (sprintf "Unknown instruction: %i" program.[pc])
        |> ignore

    printfn "%A" program
    
run [|1;1;1;4;99;5;6;0;99|]