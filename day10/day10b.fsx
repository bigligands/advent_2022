open System.IO
open System

let input = File.ReadAllLines(@"./day10/input.txt") |> List.ofArray

type Instruction =
    | Noop of int
    | Add of int

let (|IsAdd|_|) line =
    match line |> Seq.toList with
    | 'a' :: 'd' :: 'd' :: 'x' :: ' ' :: value -> Some (Add(value |> String.Concat |> int))
    | _ -> None

let (|IsNoop|_|) line =
    match line with
    | "noop" -> Some (Noop (0))
    | _ -> None

let mutable screen: string list = []
let cycles = [ 1..241 ]

let iterate_cycles cycles input =
    let rec cycle cycles input buffer b_count register cycle_count =

        let is_drawn = [register - 1 .. register + 1] |> Seq.contains ((cycle_count-2) % 40)
        let pixel = 
            match is_drawn with
            |true -> "#"
            |false -> "."
        screen <- screen @ [pixel]

        match cycles with
        | _ :: remaining_cycles ->
            if b_count > 0 then 
                cycle remaining_cycles input buffer (b_count - 1) register (cycle_count + 1)
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
                        | [] -> 
                            cycle remaining_cycles tail_instructions [ a ] 1 register (cycle_count + 1)
                    | IsNoop (Noop(n)) ->
                        match buffer with
                        | register_value :: tail_buffer ->
                            cycle
                                remaining_cycles
                                tail_instructions
                                (tail_buffer @ [ n ])
                                0 
                                (register + register_value) 
                                (cycle_count + 1)
                        | [] -> 
                            cycle remaining_cycles tail_instructions [ n ] 0 register (cycle_count + 1) 
                    | _ -> ()
                | _ -> ()
        | [] -> ()
    cycle cycles input [] 0 1 1

iterate_cycles cycles input

screen <- screen |> List.removeAt(0)

let display = screen |> List.splitInto(6)
let format_display = display |> List.map (fun l -> l |> String.Concat)

printfn "%A" (format_display)
