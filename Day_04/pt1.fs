open System
let readLines filePath = List.ofSeq(IO.File.ReadLines(filePath))

let splitLinesForEasierChecks (str: string): int list =
    let strList = str.Split(",") |> Array.toList
    let strList1 = strList[0].Split("-") |> Array.toList |> List.map int
    let strList2 = strList[1].Split("-") |> Array.toList |> List.map int
    strList1@strList2
    
let wing (arr: string list) =
    let rec aux acc (arr: string list) =
        match arr with
        | [] -> acc
        | x :: xs ->
            let intList = splitLinesForEasierChecks x
            if (intList[0] <= intList[2] && intList[1] >= intList[3]) || (intList[0] >= intList[2] && intList[1] <= intList[3])
            then aux (acc+1) xs
            else aux acc xs
    aux 0 arr

printfn "%A" (wing (readLines "./Day_04/big.txt"))