let readLines filePath =
    List.ofSeq <| System.IO.File.ReadLines filePath

[<EntryPoint>]
let main args =
    do // printfn "%d" << Day1.Day1.doPart2 <| readLines "input/day1.txt"
        // printfn "%d" << Day3.Day3.part2 <| System.IO.File.ReadAllText("input/day3.txt")
        printfn "%d" << Day10.Day10.part2 <| readLines "input/day10.txt"
    // printfn "%A" << Day9.Day9.part2
    // <| (System.IO.File.ReadAllText "input/day9.txt").Trim()

    0
