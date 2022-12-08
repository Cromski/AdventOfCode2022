open System
let readLines filePath = List.ofSeq(IO.File.ReadLines(filePath))



printfn "%A" (readLines "./Day_09/big.txt")