module AOC2019.Day2

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

    program

let program = [1;0;0;3;1;1;2;3;1;3;4;3;1;5;0;3;2;13;1;19;1;10;19;23;1;23;9;27;1;5;27;31;2;31;13;35;1;35;5;39;1;39;5;43;2;13;43;47;2;47;10;51;1;51;6;55;2;55;9;59;1;59;5;63;1;63;13;67;2;67;6;71;1;71;5;75;1;75;5;79;1;79;9;83;1;10;83;87;1;87;10;91;1;91;9;95;1;10;95;99;1;10;99;103;2;103;10;107;1;107;9;111;2;6;111;115;1;5;115;119;2;119;13;123;1;6;123;127;2;9;127;131;1;131;5;135;1;135;13;139;1;139;10;143;1;2;143;147;1;147;10;0;99;2;0;14;0]
    
let zeroToNinetyNine = [0 .. 99]
let values = List.allPairs zeroToNinetyNine zeroToNinetyNine


values
|> List.iter (fun (noun, verb) -> 
    let prog = Array.ofList program
    prog.[1] <- noun
    prog.[2] <- verb

    let output = run prog
    
    if output.[0] = 19690720 then
        printf "100 * %i + %i = %i" noun verb (100 * noun + verb)
)