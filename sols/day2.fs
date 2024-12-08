namespace Day2

module Day2 =
    let parse (lines: string list) =
        lines |> List.map (fun line -> line.Split(" ") |> Array.toList |> List.map int)

    type PartialReport =
        | Decreasing
        | Increasing
        | Invalid
        | None

    let isSafe (report: int list) : bool =
        let diffs = report |> List.pairwise |> List.map (fun (x, y) -> (x - y)) in

        diffs
        |> List.map (fun x ->
            if ((1 <= x) && (x <= 3)) then Decreasing
            elif ((-3 <= x) && (x <= -1)) then Increasing
            else Invalid)
        |> List.fold
            (fun acc x ->
                match (acc, x) with
                | (Decreasing, Decreasing) -> Decreasing
                | (Increasing, Increasing) -> Increasing
                | (None, v) -> v
                | _ -> Invalid)
            None
        |> (fun x -> x <> Invalid)

    let isSafePart2 (report: int list) : bool =
        [ 0 .. report.Length - 1 ]
        |> List.map (fun i -> List.removeAt i report)
        |> List.append [ report ]
        |> List.exists isSafe

    let part1 (reports: string list) =
        reports |> parse |> List.filter isSafe |> List.length

    let part2 (reports: string list) =
        reports |> parse |> List.filter isSafePart2 |> List.length
