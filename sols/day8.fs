namespace Day8

module Day8 =
    type Field =
        { width: int
          height: int
          antenna: Map<char, (int * int) array> }

        member this.inBounds(i: int, j: int) : bool =
            0 <= i && i < this.height && 0 <= j && j < this.width

    let parse (lines: string list) : Field =
        let width = lines.[0].Length
        let height = lines.Length

        let antennae =
            Array.allPairs [| 0 .. width - 1 |] [| 0 .. height - 1 |]
            |> Array.map (fun (x, y) -> lines.[height - 1 - y].[x], (x, y))
            |> Array.filter (fun (c, _) -> c <> '.')
            |> Array.groupBy fst
            |> Array.map (fun (c, ps) -> c, Array.map snd ps)
            |> Map.ofArray

        { width = width
          height = height
          antenna = antennae }

    let combos (arr: 'a array) =
        Array.allPairs [| 0 .. arr.Length - 1 |] [| 0 .. arr.Length - 1 |]
        |> Array.filter (fun (i, j) -> i <> j)
        |> Array.map (fun (i, j) -> arr.[i], arr.[j])

    let antinodes1 (x1, y1) (x2, y2) =
        let dx = x2 - x1
        let dy = y2 - y1
        [| x1 - dx, y1 - dy; x2 + dx, y2 + dy |]

    let antinodes2 field (x1, y1) (x2, y2) =
        let n = max field.width field.height
        let dx = x2 - x1
        let dy = y2 - y1

        [| -n .. n |]
        |> Array.map (fun i -> x1 + i * dx, y1 + i * dy)
        |> Array.filter field.inBounds

    let allAntinodes antinodes (field: Field) =
        Map.toArray field.antenna
        |> Array.map snd
        |> Array.collect combos
        |> Array.collect (fun (pos1, pos2) -> antinodes pos1 pos2)
        |> Array.distinct
        |> Array.filter field.inBounds

    let part1 (lines: string list) =
        lines |> parse |> allAntinodes antinodes1 |> Array.length

    let part2 (lines: string list) =
        let field = parse lines
        field |> allAntinodes (antinodes2 field) |> Array.length
