open System
let readLines filePath = List.ofSeq(IO.File.ReadLines(filePath))


let rec addFileSizeToAllDirInMap (map: Map<string list,int>) (path: string list) (size: int): Map<string list,int> =
    match path with
    | [] -> map
    | x :: xs -> addFileSizeToAllDirInMap (map.Add(path, map.Item(path)+size)) xs size

let wing arr =
    let rec aux (acc: Map<string list,int>) (path: string list) (arr: string list) =
        match arr with
        | []      -> acc
        | x :: xs -> 
            match x.Split(" ") with
            | s when s[0] = "$" -> 
                match s with
                | s when s[1] = "cd" && s[2] = ".." -> aux acc path.Tail xs
                | s when s[1] = "cd" -> aux (acc.Add(s[2]::path, 0)) (s[2]::path) xs
                | _ -> aux acc path xs //_[1] = "ls"
            | s when s[0] = "dir" -> aux acc path xs
            | s -> aux (addFileSizeToAllDirInMap acc path (s[0] |> int)) path xs
    aux Map.empty [] arr

let getPoints (map: Map<string list,int>) : int =
    Map.fold (fun acc _ v -> if v <= 100000 then acc+v else acc) 0 map

printfn "%A" (getPoints (wing (readLines "./Day_07/big.txt")))