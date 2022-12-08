open System
let readLines filePath = List.ofSeq(IO.File.ReadLines(filePath))

let get2dArray (arr: string list) =
    let rec aux acc (arr: string list) =
        match arr with
        | [] -> acc
        | x :: xs -> aux (acc@[(List.map (fun x -> int x - int '0') (x.ToCharArray() |> Array.toList))]) xs
    aux [] arr

let getTreeScore (coord1: int) (coord2: int) (arr: int list list): int =
    let currentTreeHeight = arr[coord1][coord2]
    
    let oneWay i arr =
        let rec aux (acc: int list) (arr: int list) =
            match arr with
            | [] -> acc
            | x :: xs when i <= x -> x::acc
            | x :: xs -> aux (x::acc) xs
        (aux [] arr).Length
    
    if coord2 <> arr.Length-1 && coord2 <> arr[0].Length-1 && coord1 <> arr.Length-1 && coord1 <> arr[0].Length-1
    then
        let up = seq {for i in 0 .. coord1-1 do yield arr[i][coord2]} |> Seq.toList
        let down = seq {for i in coord1+1 .. arr[1].Length-1 do yield arr[i][coord2]} |> Seq.toList
        let left = seq {for i in 0 .. coord2-1 do yield arr[coord1][i]} |> Seq.toList
        let right = seq {for i in coord2+1 .. arr[1].Length-1 do yield arr[coord1][i]} |> Seq.toList
        (oneWay currentTreeHeight (up |> List.rev))*(oneWay currentTreeHeight down)*(oneWay currentTreeHeight (left |> List.rev))*(oneWay currentTreeHeight right)
    else
        0

let rec checkHighestScoreInRowOfTrees (row: int list) (rowNumber: int) (arr: int list list): int =
    let rec aux (acc: int) (columnNumber: int) (row: int list) =
        match row with
        | [] -> acc
        | _ :: xs -> aux (if acc > (getTreeScore rowNumber columnNumber arr) then acc else (getTreeScore rowNumber columnNumber arr)) (columnNumber+1) xs
    aux 0 1 row

let getInteriorVisibleTreesPoint2d (arr: int list list): int =
    let cutArray = arr |> List.rev |> List.tail |> List.rev
    let rec aux acc (rowNumber: int) (arrx: int list list) =
        match arrx with
        | [] -> acc
        | x :: xs -> aux (if acc > (checkHighestScoreInRowOfTrees (x |> List.rev |> List.tail |> List.rev) rowNumber arr) then acc else (checkHighestScoreInRowOfTrees (x |> List.rev |> List.tail |> List.rev) rowNumber arr)) (rowNumber+1) xs
    aux 0 1 cutArray 

printfn "%A" (getInteriorVisibleTreesPoint2d (get2dArray (readLines "./Day_08/big.txt")))