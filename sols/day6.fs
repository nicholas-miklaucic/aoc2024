namespace Day6

module Day6 =
    type Dir =
        | Up
        | Down
        | Left
        | Right

    type Square =
        | Empty
        | Wall

    type State =
        { board: Square array array
          pos: int * int
          dir: Dir }

        member this.Guard() = this.pos, this.dir

    let inBounds (board: Square array array) (i: int, j: int) : bool =
        i >= 0 && i < board.Length && j >= 0 && j < board.[i].Length

    let get (board: Square array array) (i: int, j: int) : Square option =
        if inBounds board (i, j) then Some(board.[i].[j]) else None

    let parse (lines: string list) : State =
        let parseLine (line: string) =
            line.ToCharArray()
            |> Array.map (fun c ->
                match c with
                | '.' -> Empty
                | '#' -> Wall
                | '^' -> Empty
                | _ -> failwith "invalid char")

        let board = Array.map parseLine <| List.toArray lines

        let guard =
            List.allPairs [ 0 .. lines.Length - 1 ] [ 0 .. lines.[0].Length - 1 ]
            |> List.filter (fun (i, j) -> lines.[i].[j] = '^')

        { board = board
          pos =
            match guard with
            | [ pos ] -> pos
            | _ -> failwith "invalid guard"
          dir = Up }

    let turnCW (dir: Dir) : Dir =
        match dir with
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up

    let moveInDir (dir: Dir) (i: int, j: int) : int * int =
        match dir with
        | Up -> i - 1, j
        | Right -> i, j + 1
        | Down -> i + 1, j
        | Left -> i, j - 1

    let move (state: State) : State option =
        let newPos = moveInDir state.dir state.pos

        match get state.board newPos with
        | Some Wall -> Some { state with dir = turnCW state.dir }
        | Some Empty -> Some { state with pos = newPos }
        | None -> None

    let rec fullPath (state: State) : State list =
        match move state with
        | Some newState -> state :: fullPath newState
        | None -> [ state ]


    let isLoop (state: State) : bool =
        let rec iter (state: State) seen : bool =
            match move state with
            | Some next ->
                if Set.contains (next.Guard()) seen then
                    true
                else
                    iter next (Set.add (state.Guard()) seen)
            | None -> false in

        iter state Set.empty

    let part1 (lines: string list) : int =
        lines |> parse |> fullPath |> List.distinctBy (fun x -> x.pos) |> List.length

    let part2 (lines: string list) : int =
        let state = parse lines

        state
        |> fullPath
        |> List.map (fun x -> x.pos)
        |> List.distinct
        |> List.filter ((<>) state.pos)
        |> List.filter (fun (i, j) ->
            isLoop
                { state with
                    board = Array.updateAt i (Array.updateAt j Wall state.board.[i]) state.board })
        |> List.length
