namespace Day7

module Day7 =
    type Eqn =
        { testVal: bigint
          operands: bigint list }

    let parseLine (line: string) : Eqn =
        let ab = line.Split(": ")

        { testVal = bigint.Parse ab.[0]
          operands = ab.[1].Split(" ") |> Array.toList |> List.map bigint.Parse }

    let concat (x: bigint) (y: bigint) : bigint = bigint.Parse $"%A{x}%A{y}"
    let funcs: (bigint -> bigint -> bigint) list = [ (+); (fun x y -> x * y); concat ]

    let rec solvable (fns) (target: bigint) (vals: bigint list) =
        match vals with
        | [] -> failwith ("oops")
        | [ x ] -> x = target
        | x :: y :: xs ->
            fns
            |> List.map (fun f -> f x y)
            |> List.exists (fun z -> solvable fns target (z :: xs))

    let part1 (lines: string list) : bigint =
        lines
        |> List.map parseLine
        |> List.filter (fun eqn -> solvable (List.take 2 funcs) eqn.testVal eqn.operands)
        |> List.sumBy (fun eqn -> eqn.testVal)

    let part2 (lines: string list) : bigint =
        lines
        |> List.map parseLine
        |> List.filter (fun eqn -> solvable funcs eqn.testVal eqn.operands)
        |> List.sumBy (fun eqn -> eqn.testVal)
