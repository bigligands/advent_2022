open System.IO

let input = File.ReadAllLines("./day08/input.txt") |> List.ofArray

let build_tree_grid input =
    let rec divide_row (input: string list) (grid: list<int list>) =
        match input with
        | head :: tail ->
            let tree_row = head |> Seq.map (fun c -> int c - int '0') |> Seq.toList
            divide_row tail (grid @ [ tree_row ])
        | [] -> grid

    divide_row input []

let tree_grid = build_tree_grid input

let check_view (pos: int * int) (rows: int list) (cols: int list) =

    let check_left_right (pos: int) (rows: int list) =

        let left =
            match pos with
            | 0 -> 0
            | d ->
                match rows[.. d - 1] |> List.rev |> List.tryFindIndex (fun tree -> tree >= rows[d]) with
                | Some x -> x + 1
                | None -> rows[.. d - 1].Length

        let right =
            match pos with
            | x when x = rows.Length - 1 -> 0
            | d ->
                match rows[d + 1 ..] |> List.tryFindIndex (fun tree -> tree >= rows[d]) with
                | Some x -> x + 1
                | None -> rows[d + 1 ..].Length

        left * right


    let check_up_down (pos: int) (cols: int list) =

        let up =
            match pos with
            | 0 -> 0
            | d ->
                match cols[.. d - 1] |> List.rev |> List.tryFindIndex (fun tree -> tree >= cols[d]) with
                | Some x -> x + 1
                | None -> cols[.. d - 1].Length

        let down =
            match pos with
            | x when x = rows.Length - 1 -> 0
            | d ->
                match cols[d + 1 ..] |> List.tryFindIndex (fun tree -> tree >= cols[d]) with
                | Some x -> x + 1
                | None -> cols[d + 1 ..].Length

        up * down

    let visible_trees =
        (check_left_right (fst pos) rows) * (check_up_down (snd pos) cols)

    visible_trees



let scan_for_visible_trees grid =
    let rec check_position (visible: list<int>) (row: int) (grid_rows: list<int list>) (grid_cols: list<int list>) =
        match grid_rows with
        | head :: tail ->
            let visible_trees =
                head |> List.mapi (fun i _ -> check_view (i, row) head grid_cols[i]) |> List.max

            check_position (visible @ [ visible_trees ]) (row + 1) tail grid_cols
        | [] -> visible

    check_position [ 0 ] 0 tree_grid (tree_grid |> List.toSeq |> List.transpose)

let scenic_score = scan_for_visible_trees tree_grid |> List.max
printfn "best scenic score: %d" scenic_score
