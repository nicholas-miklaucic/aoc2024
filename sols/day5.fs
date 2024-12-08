namespace Day5

module Day5 =
    let parse (lines: string list) =
        let rules, manuals = List.splitAt (List.findIndex ((=) "") lines) lines

        let rules =
            rules
            |> List.map (fun rule -> let ab = rule.Split('|', 2) in int ab.[0], int ab.[1])

        let manuals =
            manuals
            |> List.skip 1
            |> List.map (fun man -> man.Split ',' |> Array.toList |> List.map int)

        rules, manuals

    let follows (rule: int * int) (manual: int list) : bool =
        let a, b = rule in

        Option.map2 (<=) (List.tryFindIndex ((=) a) manual) (List.tryFindIndex ((=) b) manual)
        |> Option.defaultValue true

    let followsAll rules manual =
        rules |> List.forall (fun rule -> follows rule manual)

    let swap (l: int list) (a: int) (b: int) =
        let i = List.findIndex ((=) a) l
        let j = List.findIndex ((=) b) l
        l |> List.updateAt i b |> List.updateAt j a

    let rec topSort (rules: (int * int) list) (manual: int list) : int list =
        if followsAll rules manual then
            manual
        else
            let a, b = rules |> List.find (fun rule -> follows rule manual |> not) in topSort rules (swap manual a b)

    let part1 (line: string list) =
        let rules, manuals = parse line

        manuals
        |> List.filter (followsAll rules)
        |> List.map (fun l -> l.[l.Length / 2])
        |> List.sum

    let part2 (line: string list) =
        let rules, manuals = parse line

        manuals
        |> List.filter (not << followsAll rules)
        |> List.map (topSort rules)
        |> List.map (fun l -> l.[l.Length / 2])
        |> List.sum
