let readLines filePath =
    List.ofSeq <| System.IO.File.ReadLines filePath

[<EntryPoint>]
let main args =
    do // printfn "%d" << Day1.Day1.doPart2 <| readLines "input/day1.txt"
        // printfn "%d" << Day3.Day3.part2 <| System.IO.File.ReadAllText("input/day3.txt")
        // printfn "%d" << Day5.Day5.part2 <| readLines "input/day5.txt"
        printfn "%A" << Day7.Day7.part2 <| readLines "input/day7.txt"

    0
