let readLines filePath = List.ofSeq (System.IO.File.ReadLines(filePath))
let wing arr =
    let rec aux acc arr =
        match arr with
        | [] -> acc
        | x :: xs ->
            match x with
            | "A X" -> aux (acc+4) xs
            | "B X" -> aux (acc+1) xs
            | "C X" -> aux (acc+7) xs
            | "A Y" -> aux (acc+8) xs
            | "B Y" -> aux (acc+5) xs
            | "C Y" -> aux (acc+2) xs
            | "A Z" -> aux (acc+3) xs
            | "B Z" -> aux (acc+9) xs
            | "C Z" -> aux (acc+6) xs
            | _ -> aux acc xs
    aux 0 arr
printfn "%A" (wing (readLines "/Users/Cromski/Documents/Freetime/AdventOfCode2022/Day_02/big.txt"))