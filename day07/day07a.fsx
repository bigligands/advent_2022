open System.IO

let input = File.ReadAllLines("input.txt") |> Array.toList

type Cmd =
    | LS
    | CD of string

type StdOut =
    | Dir of string
    | File of int

type Directory =
    { Current: string list
      Tree: Map<string list, int> } 

let parse_command cmd =
    match (cmd |> List.ofSeq) with
    | '$' :: ' ' :: 'c' :: 'd' :: ' ' :: dir -> CD((dir |> Seq.map string |> String.concat ""))
    | _ -> LS

let (|Command|_|) (line: string) =
    match line.Split(' ') |> Array.head with
    | "$" -> Some(parse_command line)
    | _ -> None

let (|Output|_|) (line: string) =
    match (line.Split(' ') |> List.ofArray) with
    | "dir" :: dir -> Some(Dir(dir.Head))
    | size :: _ -> Some(File(size |> int))
    | [] -> None

let rec scan_subdir (par : list<string>) (sub : list<string>) =
    match par with
    | par_head :: par_tail ->
        match sub with
        | sub_head :: sub_tail ->
            if (par_head = sub_head) then scan_subdir par_tail sub_tail else false  
        | [] -> false // if sub empty before parent then its not a subdirectory 
    | [] -> true

/// Finds the sum of the Size of all subdirectories
let sum_subdirectories (tree: Map<string list, int>) (dir: string list) =
    tree
    |> Map.filter (fun subdir _ ->
        scan_subdir dir subdir)
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.sum


let build_tree (input: string list) =
    let rec parse_tree (directory: Directory) (input: string list) =
        match input with
        | line :: tail ->
            match line with
            | Command cmd ->
                match cmd with
                | LS -> 
                    // continue the loop
                    parse_tree directory tail
                | CD dir -> 
                    match dir with
                    | ".." ->
                        // pop one off the current directory to go up one level
                        let cd = directory.Current |> List.take (directory.Current.Length - 1)
                        parse_tree {Current = cd; Tree = directory.Tree} tail
                    | d ->
                        // change current directory to dir (update Current = []@dir)
                        let cd = directory.Current @ [d] 
                        parse_tree {Current = cd; Tree = directory.Tree} tail
            | Output out ->
                match out with
                | Dir dir -> 
                    // add new directory to the tree and send it through the loop
                    let new_dir = directory.Tree |> Map.add (directory.Current @ [dir]) 0
                    parse_tree {Current = directory.Current; Tree = new_dir} tail
                | File file -> 
                    // add the file size to the current directory
                    let updated_dir = 
                        directory.Tree 
                        |> Map.change directory.Current (fun x -> 
                            match x with
                            | Some size -> Some(size + file)
                            | None -> None)
                    parse_tree {Current = directory.Current; Tree = updated_dir} tail
            | _ -> parse_tree directory tail
        | [] -> directory
    parse_tree { Current = []; Tree = ([(["/"], 0)] |> Map) } input

let tree = build_tree input

// need to recursively apply the sum of subdirectories over all directories
let tree_accumulated = 
    tree.Tree
    |> Map.map (fun k _ ->
        sum_subdirectories tree.Tree k)

// used to view directory structure
let string_dirs =
    tree_accumulated.Keys
    |> Seq.map (fun k -> k |> List.toSeq |> String.concat "/")

let filtered_sum =
    tree_accumulated
    |> Map.values
    |> Seq.filter (fun size -> size <= 100_000)
    |> Seq.sum

printfn "Sum of directories: %d" filtered_sum