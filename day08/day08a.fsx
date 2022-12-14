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

let is_visible (pos: int * int) (rows: int list) (cols: int list) =
    let check_left_right (pos: int) (rows: int list) =
        match pos with
        | 0 -> true
        | edge when edge = rows.Length - 1 -> true
        | _ ->
            let trees_betwixt_left =
                rows[.. pos - 1] |> List.tryFind (fun tree -> tree >= rows[pos])

            let trees_betwixt_right =
                rows[pos + 1 ..] |> List.tryFind (fun tree -> tree >= rows[pos])

            if trees_betwixt_left.IsNone || trees_betwixt_right.IsNone then
                true
            else
                false

    let check_up_down (pos: int) (cols: int list) =
        match pos with
        | 0 -> true
        | edge when edge = cols.Length - 1 -> true
        | _ ->
            let trees_betwixt_up =
                cols[.. pos - 1] |> List.tryFind (fun tree -> tree >= cols[pos])

            let trees_betwixt_down =
                cols[pos + 1 ..] |> List.tryFind (fun tree -> tree >= cols[pos])

            if trees_betwixt_up.IsNone || trees_betwixt_down.IsNone then
                printfn ""
                true
            else
                false

    if
        (check_left_right (fst pos) rows) = true
        || (check_up_down (snd pos) cols) = true
    then
        true
    else
        false

let scan_for_visible_trees grid =
    let rec check_position (visible: int) (row: int) (grid_rows: list<int list>) (grid_cols: list<int list>) =
        match grid_rows with
        | head :: tail ->
            let visible_trees =
                head
                |> List.mapi (fun i _ -> is_visible (i, row) head grid_cols[i])
                |> List.filter (fun b -> b = true)
                |> List.length

            check_position (visible + visible_trees) (row + 1) tail grid_cols
        | [] -> visible

    check_position 0 0 tree_grid (tree_grid |> List.toSeq |> List.transpose)

let vis = scan_for_visible_trees tree_grid
printfn "%d" vis
