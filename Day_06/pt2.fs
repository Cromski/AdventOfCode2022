open System
let readLines filePath = List.ofSeq(IO.File.ReadLines(filePath))

let rec trueIfAllCharsAreDifferent (str: string) =
    let cList = str.ToCharArray() |> Array.toList
    let rec aux (cList: char list) =
        match cList with
        | [] -> true
        | x :: xs when String.Concat(Array.ofList(xs)).Contains x = true -> false
        | x :: xs when String.Concat(Array.ofList(xs)).Contains x = false -> aux xs
    aux cList

let wing (strList: string list) =
    let rec aux (acc: int) (strList: string list) = 
        match strList with
        | x :: _ when trueIfAllCharsAreDifferent (x.Substring(0,14)) -> acc+14
        | x :: _ -> aux (acc+1) [(x.Substring(1,x.Length-1))]
        | _ -> 0 // never happens
    aux 0 strList

printfn "%A" (wing (readLines "./Day_06/big.txt"))