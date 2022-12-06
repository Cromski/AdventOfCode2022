open System
let readLines filePath = List.ofSeq(IO.File.ReadLines(filePath))

let get2dArray (arr: string list) =
    let rec helper (acc: char list) (str: string) (i: int) =
        match str with
        | _ when i > str.Length -> acc
        | s  -> helper (acc@[s[i]]) str (i+4)
    
    let rec aux (acc: char list list) (arr: string list) =
        match arr with
        | s :: _ when s = "" -> acc
        | s :: xs  -> aux (acc@[(helper [] s 1)]) xs
        | _ -> [] // will never happen
    aux [] arr

let rotate2dArray (arr: char list list) =
    let seq = seq{ for i in 0 .. (arr |> List.rev).Head.Length-1 -> (
                    let rec getOneLine (acc: char list) (arr: char list list) (i: int) =
                        match arr with
                        | [] -> acc
                        | x :: xs -> getOneLine (acc@[x[i]]) xs (i)
                    getOneLine [] (arr |> List.rev) i
        )}
    Seq.toList seq
    
let crop2dArray (arr: char list list) =
    let rec aux acc arr = 
        match arr with
        | [] -> acc
        | x :: xs when (x |> List.rev)[0] = ' ' -> aux acc ( ((x |> List.rev).Tail |> List.rev) ::xs)
        | x :: xs -> aux (acc@[x]) xs
    aux [] arr

let getInstructions (arr: string list) =
    let rec aux (acc: (int*int*int) list) (arr: string list) =
        match arr with
        | [] -> acc
        | x :: xs when x = "" -> aux acc xs
        | x :: xs when x[0] = ' ' || x[0] = '[' -> aux acc xs
        | x :: xs ->
            let splitX = x.Split(' ')
            aux (acc@[(splitX[1] |> int, splitX[3] |> int, splitX[5] |> int)]) xs
    aux [] arr

let moveFromTo (arr: char list list) (amount,from,too)(*(amount: int) (from: int) (too: int)*) =
    let thisWillGetMoved = String.Concat(Array.ofList(arr[from-1] |> List.rev)).Substring(0,amount).ToCharArray() |> Array.toList
    let movedFrom = String.Concat(Array.ofList(arr[from-1])).Substring(0,(arr[from-1].Length)-amount).ToCharArray() |> Array.toList
    let movedTo = arr[too-1]@thisWillGetMoved
    
    let rec buildArray acc (arr: char list list) =
        match arr with
        | [] -> acc
        | x :: xs when x[0] = movedFrom[0] -> buildArray (acc@[movedFrom]) xs
        | x :: xs when x[0] = movedTo[0] -> buildArray (acc@[movedTo]) xs
        | x :: xs -> buildArray (acc@[x]) xs
    buildArray [] arr
    
let runInstructions (inst: (int*int*int) list) (arr: char list list) =
    let rec aux acc inst =
        match inst with
        | [] -> acc
        | x :: xs -> aux (moveFromTo acc x) xs 
    aux arr inst

let getLetters (arr: char list list) =
    let rec aux acc (arr: char list list) =
        match arr with
        | [] -> acc
        | x :: xs -> aux (acc@[x |> List.last]) xs
    String.Concat(Array.ofList(aux [] arr)) 


printfn "%A" (getLetters (runInstructions (getInstructions (readLines "./Day_05/big.txt")) (crop2dArray (rotate2dArray (get2dArray (readLines "./Day_05/big.txt"))))))
