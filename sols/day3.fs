namespace Day3

open System.Text.RegularExpressions

module Day3 =
    let mul_rx_pat = @"mul\(([0-9]{1,3}),([0-9]{1,3})\)"
    let mul_rx = Regex(mul_rx_pat, RegexOptions.Compiled)

    let part1 (text: string) : int =
        mul_rx.Matches(text)
        |> Seq.map (fun m -> int m.Groups.[1].Value * int m.Groups.[2].Value)
        |> Seq.sum

    let start_mul_rx = Regex("^" + mul_rx_pat, RegexOptions.Compiled)
    let do_rx = Regex(@"^do\(\)", RegexOptions.Compiled)
    let dont_rx = Regex(@"^don't\(\)", RegexOptions.Compiled)

    type MulState =
        | Enabled
        | Disabled

    type ParserState =
        { mul_state: MulState
          sum: int
          text: string }

    let dont_step (state: ParserState) : ParserState =
        match dont_rx.Matches(state.text) |> Seq.toList with
        | [] -> state
        | [ m ] ->
            { state with
                text = state.text.Substring(m.Length)
                mul_state = Disabled }
        | _ -> failwith "Multiple matches for don't, which shouldn't be possible"

    let do_step (state: ParserState) : ParserState =
        match do_rx.Matches(state.text) |> Seq.toList with
        | [] -> state
        | [ m ] ->
            { state with
                text = state.text.Substring(m.Length)
                mul_state = Enabled }
        | _ -> failwith "Multiple matches for do, which shouldn't be possible"

    let mul_step (state: ParserState) : ParserState =
        match start_mul_rx.Matches(state.text) |> Seq.toList with
        | [] -> state
        | [ m ] ->
            { state with
                text = state.text.Substring(m.Length)
                sum =
                    if state.mul_state = Enabled then
                        state.sum + (int m.Groups.[1].Value * int m.Groups.[2].Value)
                    else
                        state.sum }
        | _ -> failwith "Multiple matches for mul, which shouldn't be possible"

    let step (state: ParserState) : ParserState =
        let stepped = state |> dont_step |> do_step |> mul_step

        if stepped.text = state.text then
            { stepped with
                text = stepped.text.Substring(1) }
        else
            stepped

    let rec stepThrough (state: ParserState) : int =
        if state.text.Length = 0 then
            state.sum
        else
            state |> step |> stepThrough

    let part2 (text: string) : int =
        stepThrough
            { mul_state = Enabled
              sum = 0
              text = text }
