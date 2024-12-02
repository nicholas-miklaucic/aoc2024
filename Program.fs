let readLines filePath =
    List.ofSeq <| System.IO.File.ReadLines(filePath)

[<EntryPoint>]
let main args =
    do printfn "%d" << Day1.Day1.doPart2 <| readLines "input/day1.txt"

    0
