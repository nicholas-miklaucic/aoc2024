namespace Day9

module Day9 =
    let parse (inp: string) =
        inp.ToCharArray()
        |> Array.map (fun c -> int (c.ToString()))
        |> Array.mapi (fun i n ->
            if i % 2 = 0 then
                Array.replicate n (Some(i / 2))
            else
                Array.replicate n None)
        |> Array.concat

    let show (inp: int option array) =
        inp
        |> Array.map (Option.map string)
        |> Array.map (Option.defaultValue ".")
        |> String.concat ""

    let rec defrag1' (i: int) (j: int) (inp: int option array) =
        // printfn "%A %A\t%A" inp.[i] inp.[j] (show inp)

        if i >= j then
            inp
        else
            match inp.[i] with
            | Some _ -> defrag1' (i + 1) j inp
            | None ->
                match inp.[j] with
                | Some _ -> defrag1' (i + 1) (j - 1) (inp |> Array.updateAt i inp.[j] |> Array.updateAt j None)
                | None -> defrag1' i (j - 1) inp

    let defrag1 (inp: int option array) = defrag1' 0 (inp.Length - 1) inp

    type Block = { id: int option; size: int }

    let parse2 (inp: string) =
        inp.ToCharArray()
        |> Array.mapi (fun i c ->
            let n = int (c.ToString()) in

            { id = if i % 2 = 0 then Some(i / 2) else None
              size = n })

    let toFullArr = Array.collect (fun b -> Array.replicate b.size b.id)

    let rec defrag2' (i: int) (j: int) (blocks: Block array) =
        // printfn "%A" <| (show <| toFullArr blocks)
        // printfn "%A %A" blocks.[i] blocks.[j]

        if j = 0 then
            blocks
        elif i >= j then
            defrag2' 0 (j - 1) blocks
        else
            let si = blocks.[i].size
            let sj = blocks.[j].size

            match blocks.[i].id, blocks.[j].id with
            | Some _, _ -> defrag2' (i + 1) j blocks
            | None, None -> defrag2' i (j - 1) blocks
            | None, Some _ ->
                if sj <= si then
                    defrag2'
                        0
                        j
                        (blocks
                         |> Array.updateAt i blocks.[j]
                         |> Array.updateAt j { blocks.[j] with id = None }
                         |> Array.insertAt (i + 1) { id = None; size = si - sj })
                else
                    defrag2' (i + 1) j blocks

    let defrag2 (inp: Block array) = defrag2' 0 (inp.Length - 1) inp

    let checksum inp =
        inp
        |> Array.mapi (fun i k ->
            match k with
            | Some v -> bigint (i * v)
            | None -> bigint 0)
        |> Array.sum

    let part1 (inp: string) =
        let defragged = inp |> parse |> defrag1
        // printfn "%A" (show defragged)
        checksum defragged

    let part2 (inp: string) =
        let defragged = inp |> parse2 |> defrag2 |> toFullArr
        // printfn "%A" (show defragged)
        checksum defragged
