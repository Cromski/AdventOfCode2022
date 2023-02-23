open System
let readLines filePath = List.ofSeq(IO.File.ReadLines(filePath))

type Direction =
    | U = 0
    | R = 1
    | D = 2
    | L = 3
    | None = 4

type H = int*int*int*int*Direction //x, y, prevX, prevY, dir
type T = int*int*Set<int*int> //x, y, visitedCoords
type Instruction = Direction*int //dir, moveAmount
        
let moveHeadFromInst (inst: Instruction) (Hh: H) (Tt: T): H*T =
    
    let tailMoveIfNecessary (H: H) (T: T): T =
        let Hx, Hy, HprevX, HprevY, Hdir = H
        let Tx, Ty, Tset = T
        if abs(Hx-Tx) > 1 || abs(Hy-Ty) > 1 then (HprevX, HprevY, Tset.Add(HprevX, HprevY)) else T
    
    let rec aux (acc: H*T) (inst: Instruction) =
        let H, T = acc
        let Hx, Hy, HprevX, HprevY, Hdir = H
        let Tx, Ty, Tset = T
        match inst with
        | dir, i when i > 0 ->
            match dir with
            | Direction.U -> aux ((Hx,Hy+1,Hx,Hy,dir),tailMoveIfNecessary (Hx,Hy+1,Hx,Hy,dir) T ) (dir,i-1)
            | Direction.R -> aux ((Hx+1,Hy,Hx,Hy,dir),tailMoveIfNecessary (Hx+1,Hy,Hx,Hy,dir) T ) (dir,i-1)
            | Direction.D -> aux ((Hx,Hy-1,Hx,Hy,dir),tailMoveIfNecessary (Hx,Hy-1,Hx,Hy,dir) T ) (dir,i-1)
            | Direction.L -> aux ((Hx-1,Hy,Hx,Hy,dir),tailMoveIfNecessary (Hx-1,Hy,Hx,Hy,dir) T ) (dir,i-1)
        | _ -> acc
    aux (Hh,Tt) inst


let giveHeadInstructions (arr: string list) =
    
    let getEnumFromString (str: string) : Direction =
        match str with
        | "U" -> Direction.U
        | "R" -> Direction.R
        | "D" -> Direction.D
        | "L" -> Direction.L
        | _ -> Direction.None // never happens
    
    let rec aux (acc: H*T) (arr: string list): H*T =
        let H, T = acc
        match arr with
        | [] -> acc
        | x :: xs -> aux (moveHeadFromInst (x.Split(" ")[0] |> getEnumFromString,x.Split(" ")[1] |> int) H T) xs
        
    aux ((0,0,0,0,Direction.None),(0,0,Set.empty.Add(0,0))) arr
        
let getTailVisits (HT: H*T): int =
    let _, T = HT
    let _, _, Tset = T
    Tset.Count

printfn "%A" (getTailVisits (giveHeadInstructions(readLines "./Day_09/big.txt")))