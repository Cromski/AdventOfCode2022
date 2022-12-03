open System
let readLines filePath = List.ofSeq(IO.File.ReadLines(filePath))
let rec whichCharIsInCommon (str1: string) (str2: string) (str3: string) : char =
    let str1Array = str1.ToCharArray() |> Array.toList
    match str1Array with
    | x :: _ when str2.Contains x && str3.Contains x -> x
    | _ :: xs -> whichCharIsInCommon (String.Concat(Array.ofList(xs))) str2 str3
    | _ -> 'å' // pattern will never be hit
    
let findSharedCharsInThreeRucksacks (arr: string list): char list =
    let rec aux (acc: char list) (arr: string list) =
        match arr with
        | [] -> acc
        | x :: y :: z :: xs -> aux (acc@[(whichCharIsInCommon x y z)]) xs
        | _ -> [] // pattern will never be hit with this data set
    aux [] arr

let getPointsFromCharList (cList: char list): int =
    let rec aux (acc: int) (arr: char list): int =
        match arr with
        | [] -> acc
        | x :: xs when Char.IsLower x -> aux ((Convert.ToInt32(x)-96)+acc) xs
        | x :: xs -> aux ((Convert.ToInt32(x)-38)+acc) xs
    aux 0 cList
        
printfn "%A" (getPointsFromCharList (findSharedCharsInThreeRucksacks (readLines "./Day_03/big.txt")))