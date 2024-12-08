namespace Day1

module Day1 =
    let parse (lines: string list) =
        lines
        |> List.map (fun line -> let ab = line.Split("   ") in (int (ab.[0].Trim()), int (ab.[1].Trim())))
        |> List.unzip

    let part1 (l1: int list, l2: int list) =
        List.zip (List.sort l1) (List.sort l2)
        |> List.map (fun (x, y) -> abs (x - y))
        |> List.sum

    let part2 (l1: int list, l2: int list) =
        List.allPairs l1 l2
        |> List.map (fun (x, y) -> if x = y then y else 0)
        |> List.sum



    let doPart1 = parse >> part1
    let doPart2 = parse >> part2
