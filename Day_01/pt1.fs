let readLines filePath = List.ofSeq (System.IO.File.ReadLines(filePath))

let wing arr =
    let rec aux acc arr =
        match arr with
        | [] -> acc
        | x :: xs -> 
            match x with
            | "" -> aux (0::acc) xs
            | x -> aux ((acc.Head + (x |> int))::acc.Tail) xs

    aux [0] arr

printfn "%A" (List.max (wing (readLines "/Users/Cromski/Documents/Freetime/AdventOfCode2022/Day_01/pt1-big.txt")))
