open System
let readLines filePath = List.ofSeq(IO.File.ReadLines(filePath))

let rec whichCharIsInCommon (str1: string) (str2: string) : char =
    let str1Array = str1.ToCharArray() |> Array.toList
    match str1Array with
    | x :: _ when str2.Contains x ->x
    | _ :: xs ->whichCharIsInCommon (String.Concat(Array.ofList(xs))) str2
    | _ -> '['
    
let findSharedChars (arr: string list): char list =
    let rec aux (acc: char list) (arr: string list) =
        match arr with
        | [] -> acc
        | x :: xs ->
            let strMedian = x.Length/2
            let str1 = x.Substring(0,strMedian)
            let str2 = x.Substring(strMedian,strMedian)
            aux (acc@[(whichCharIsInCommon str1 str2)]) xs
    aux [] arr

let getPointsFromCharList (cList: char list): int =
    let rec aux (acc: int) (arr: char list): int =
        match arr with
        | [] -> acc
        | x :: xs when Char.IsLower x -> aux ((Convert.ToInt32(x)-96)+acc) xs
        | x :: xs -> aux ((Convert.ToInt32(x)-38)+acc) xs
    aux 0 cList
        
printfn "%A" (getPointsFromCharList (findSharedChars (readLines "/Users/Cromski/Documents/Freetime/AdventOfCode2022/Day_03/big.txt")))