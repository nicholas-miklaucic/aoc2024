namespace Day4

module Day4 =
    let parse (lines: string list) : char list list =
        lines |> List.map (fun line -> line.ToCharArray() |> Array.toList)

    let dirs = List.allPairs [ -1 .. 1 ] [ -1 .. 1 ]

    let is_valid (board: char list list) (i: int, j: int) : bool =
        0 <= i && i < board.Length && 0 <= j && j < board.[i].Length

    let get_char (board: char list list) (i: int, j: int) : char option =
        if is_valid board (i, j) then Some board.[i].[j] else None

    let all_posns (board: char list list) =
        List.allPairs [ 0 .. board.Length - 1 ] [ 0 .. board.[0].Length - 1 ]

    let query = "XMAS".ToCharArray()
    let inds = [ 0 .. (query.Length - 1) ]

    let has_xmas (board: char list list) (i: int, j: int) (di: int, dj: int) =
        inds
        |> List.collect (fun k -> get_char board (i + di * k, j + dj * k) |> Option.toList)
        |> Array.ofList
        |> (=) query
    // let chars = seq {
    //     for ind in inds do
    //         match get_char board (i + di * ind, j + dj * ind) with
    //         | None -> ()
    //         | Some c -> yield c
    // } in
    // Array.ofSeq chars = query

    let x_inds = [ [ -1, -1; 1, 1 ]; [ 1, -1; -1, 1 ] ]

    let has_x_mas (board: char list list) (i: int, j: int) =
        if get_char board (i, j) <> Some 'A' then
            false
        else
            let chars =
                x_inds
                |> List.map (fun inds ->
                    inds
                    |> List.map (fun (di, dj) -> Option.defaultValue 'a' (get_char board (i + di, j + dj)))
                    |> List.sort) in

            chars = [ [ 'M'; 'S' ]; [ 'M'; 'S' ] ]

    let part1 (board: string list) : int =
        let board = parse board in

        List.allPairs (all_posns board) dirs
        |> List.filter (fun (pos, dir) -> has_xmas board pos dir)
        |> List.length

    let part2 (board: string list) : int =
        let board = parse board in

        all_posns board |> List.map (has_x_mas board) |> List.filter id |> List.length
