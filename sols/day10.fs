namespace Day10

module Day10 =
    [<StructuredFormatDisplay "({i},{j})">]
    type Coord =
        { i: int
          j: int }

        member this.neighbors =
            [| { i = this.i - 1; j = this.j }
               { i = this.i + 1; j = this.j }
               { i = this.i; j = this.j - 1 }
               { i = this.i; j = this.j + 1 } |]


    type Board<'T> =
        { width: int
          height: int
          data: 'T array }

        member this.neighbors(c: Coord) =
            c.neighbors |> Array.filter this.inBounds

        member this.at(c: Coord) : 'T = this.data.[c.i * this.width + c.j]

        member this.get(c: Coord) : 'T option =
            if this.inBounds c then Some(this.at c) else None

        member this.inBounds(c: Coord) : bool =
            0 <= c.i && c.i < this.height && 0 <= c.j && c.j < this.width

        member this.allCoords =
            Array.allPairs [| 0 .. this.height - 1 |] [| 0 .. this.width - 1 |]
            |> Array.map (fun (i, j) -> { i = i; j = j })

        static member parse (entry: char -> 'T) (lines: string list) : Board<'T> =
            let width = lines.[0].Length
            let height = lines.Length

            { width = width
              height = height
              data =
                lines
                |> List.map (fun line -> line.ToCharArray() |> Array.map entry)
                |> Array.concat }

    let hikingNeighbors (board: Board<int>) c =
        board.neighbors c |> Array.filter (fun c2 -> board.at c2 - 1 = board.at c)

    let rec trailsFrom (board: Board<int>) c : Coord list list =
        if board.at c = 9 then
            [ [ c ] ]
        else
            hikingNeighbors board c
            |> Array.toList
            |> List.collect (trailsFrom board)
            |> List.map (fun path -> [ c ] @ path)

    let allTrails (board: Board<int>) =
        board.allCoords
        |> Array.toList
        |> List.collect (fun c -> if board.at c = 0 then trailsFrom board c else [])

    let part1 (lines: string list) =
        let board = Board.parse (string >> int) lines
        let trails = allTrails board

        // printf "%A" trails

        trails
        |> List.distinctBy (fun path -> path.[0], path.[path.Length - 1])
        |> List.length

    let part2 (lines: string list) =
        let board = Board.parse (string >> int) lines

        board |> allTrails |> List.length
