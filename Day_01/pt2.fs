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

let rec addTop3 arr =
    let rec aux arr acc iterCount = 
        match arr with
        | x :: xs when iterCount < 3 -> aux xs (acc+x) (iterCount+1)
        | _ -> acc
    aux arr 0 0

printfn "%A" (addTop3 (List.sortDescending (wing (readLines "/Users/Cromski/Documents/Freetime/AdventOfCode2022/Day_01/pt1-big.txt"))))