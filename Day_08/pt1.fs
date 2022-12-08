open System
let readLines filePath = List.ofSeq(IO.File.ReadLines(filePath))

let get2dArray (arr: string list) =
    let rec aux acc (arr: string list) =
        match arr with
        | [] -> acc
        | x :: xs -> aux (acc@[(List.map (fun x -> int x - int '0') (x.ToCharArray() |> Array.toList))]) xs
    aux [] arr

let isTreeVisible (coord1: int) (coord2: int) (arr: int list list): int =
    let currentTreeHeight = arr[coord1][coord2]
    if coord2 <> arr.Length-1 && coord2 <> arr[0].Length-1 && coord1 <> arr.Length-1 && coord1 <> arr[0].Length-1
    then
        let up = seq {for i in 0 .. coord1-1 do yield arr[i][coord2]} |> Seq.toList
        let down = seq {for i in coord1+1 .. arr[1].Length-1 do yield arr[i][coord2]} |> Seq.toList
        let left = seq {for i in 0 .. coord2-1 do yield arr[coord1][i]} |> Seq.toList
        let right = seq {for i in coord2+1 .. arr[1].Length-1 do yield arr[coord1][i]} |> Seq.toList
        if
            List.fold (fun acc x -> currentTreeHeight > x && acc) true up ||
            List.fold (fun acc x -> currentTreeHeight > x && acc) true down ||
            List.fold (fun acc x -> currentTreeHeight > x && acc) true left ||
            List.fold (fun acc x -> currentTreeHeight > x && acc) true right 
        then 1
        else 0
    else
        0

let rec checkRowOfTrees (row: int list) (rowNumber: int) (arr: int list list): int =
    let rec aux (acc: int) (columnNumber: int) (row: int list) =
        match row with
        | [] -> acc
        | _ :: xs -> aux (acc+(isTreeVisible rowNumber columnNumber arr)) (columnNumber+1) xs
    aux 0 1 row

let getInteriorVisibleTreesAmount (arr: int list list) =
    let cutArray = arr |> List.rev |> List.tail |> List.rev
        
    let rec aux acc (rowNumber: int) (arrx: int list list) =
        match arrx with
        | [] -> acc
        | x :: xs -> aux ((checkRowOfTrees (x |> List.rev |> List.tail |> List.rev) rowNumber arr)+acc) (rowNumber+1) xs
    aux 0 1 cutArray 

let getAllVisibleTreesAmount (arr: int list list) =
    ((arr.Length*2)+(arr[0].Length-2)*2) + getInteriorVisibleTreesAmount arr

printfn "%A" (getAllVisibleTreesAmount (get2dArray (readLines "./Day_08/big.txt")))