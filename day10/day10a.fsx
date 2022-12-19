open System.IO
open System

let input = File.ReadAllLines(@"./day10/input.txt") |> List.ofArray

type Instruction =
    | Noop of int
    | Add of int

let (|IsAdd|_|) line =
    match line |> Seq.toList with
    | 'a' :: 'd' :: 'd' :: 'x' :: ' ' :: value -> Some(Add(value |> String.Concat |> int))
    | _ -> None

let (|IsNoop|_|) line =
    match line with
    | "noop" -> Some (Noop(0))
    | _ -> None

let mutable cache = []
let cycles = [ 0..220 ]

let iterate_cycles cycles input =
    let rec cycle cycles input buffer b_count register cycle_count =
        if ([ 20; 60; 100; 140; 180; 220 ] |> List.contains cycle_count) then
            do 
                cache <- cache @ [ cycle_count * register ]
                printfn "cycle count is %d" cycle_count
                printfn "register value is %d" register
        match cycles with
        | _ :: remaining_cycles ->
            if b_count > 0 then
                do cycle remaining_cycles input buffer (b_count - 1) register (cycle_count + 1)
            else
                match input with
                | cmd :: tail_instructions ->
                    match cmd with
                    | IsAdd (Add(a)) -> // match constructor to get value
                        match buffer with
                        | register_value :: tail_buffer ->
                            cycle 
                                remaining_cycles // cycles
                                tail_instructions  // input
                                (tail_buffer @ [ a ])  // buffer
                                1  // b_count
                                (register + register_value) //register
                                (cycle_count + 1) // cycle_count
                        | [] -> cycle remaining_cycles tail_instructions [ a ] 1 register (cycle_count + 1)
                    | IsNoop (Noop(n)) ->
                        match buffer with
                        | register_value :: tail_buffer ->
                            cycle
                                remaining_cycles
                                tail_instructions
                                (tail_buffer @ [ 0 ])
                                0 // b_count
                                (register + register_value) //register
                                (cycle_count + 1)
                        | [] -> cycle remaining_cycles tail_instructions [ 0 ] 0 register (cycle_count + 1)
                    | _ -> ()
                | _ -> ()
        | [] -> ()

    cycle cycles input [] 0 1 0
iterate_cycles cycles input
printfn "sum of signal strengths: %d" (cache |> List.sum)
