open System
let readLines filePath = List.ofSeq(IO.File.ReadLines(filePath))

let wing (arr: string list) =
    let rec aux (acc: int list) (xValue: int) (counter: int) (limit: int) (arr: string list) =
        match arr with
        | [] -> acc
        | x :: xs when x = "noop" -> aux acc xValue (counter+1) limit xs
        | x :: xs when counter < limit -> aux acc (xValue+(x.Split(" ")[1] |> int)) (counter+2) limit xs
        | x :: xs -> aux (acc@[xValue*(limit+2)]) (xValue+(x.Split(" ")[1] |> int)) (counter+2) (limit+40) xs 
    aux [] 1 0 18 arr

printfn "%A" (List.sum (wing (readLines "./Day_10/big.txt")))