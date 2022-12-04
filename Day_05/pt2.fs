open System
let readLines filePath = List.ofSeq(IO.File.ReadLines(filePath))


printfn "%A" (wing (readLines "./Day_05/mini.txt"))