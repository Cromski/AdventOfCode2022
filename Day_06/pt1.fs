open System
let readLine filePath = List.ofSeq(IO.File.ReadAllText(filePath))

let checkIfAnyCharsAreTheSame (a: char) b c d =
    a <> b && a <> c && a <> d && b <> c && b <> d && c <> d

let wing (cList: char list) =
    let rec aux (acc: int) (cList: char list) = 
        match cList with
        | a :: b :: c :: d :: _ when checkIfAnyCharsAreTheSame a b c d -> acc+4
        | _ :: b :: c :: d :: xs -> aux (acc+1) (b::c::d::xs)
        | _ -> 0 // never happens
    aux 0 cList

printfn "%A" (wing (readLine "./Day_06/big.txt"))