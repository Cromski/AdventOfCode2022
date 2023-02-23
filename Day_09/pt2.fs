open System
let readLines filePath = List.ofSeq(IO.File.ReadLines(filePath))

open System
let readLines filePath = List.ofSeq(IO.File.ReadLines(filePath))

type Direction =
    | U = 0
    | R = 1
    | D = 2
    | L = 3
    | None = 4

type H = int*int*int*int*Direction //x, y, moveX, moveY, dir
type T = int*int*int*int*int*Set<int*int> //id, x, y, moveX, moveY, visitedCoords
type Instruction = Direction*int //dir, moveAmount

let moveRestOfTailMaybe (T: T) (Tl: T list) : T list =
    let rec aux (acc: T list) (T: T) (Tll: T list) =
        let Tid, Tx, Ty, TmoveX, TmoveY, Tset = T
        match Tll with
        | [] -> T::acc
        | (Tid2, Tx2, Ty2, TmoveX2, TmoveY2, Tset2) :: xs ->
            if abs(Tx-Tx2) > 1 || abs(Ty-Ty2) > 1
            then aux (acc@[(Tid2, Tx2+TmoveX, Ty2+TmoveY, TmoveX, TmoveY, Tset2)]) (Tid2, Tx2+TmoveX, Ty2+TmoveY, TmoveX, TmoveY, Tset2) (Tll |> List.updateAt Tid2 (Tid2, Tx2+TmoveX, Ty2+TmoveY, TmoveX, TmoveY, Tset2))
            else T::acc
    aux [] T Tl

let moveTailIfNecessary (H: H) (Tl: T list) : T list =
    printfn "%A %A" H Tl
    let Hx, Hy, HmoveX, HmoveY, Hdir = H
    let Tid, Tx, Ty, TmoveX, TmoveY, Tset = Tl.Head
    if abs(Hx-Tx) > 1 || abs(Hy-Ty) > 1
    then
        printfn "Hx: %A, Hy: %A, HmoveX: %A, HmoveY: %A" Hx Hy HmoveX HmoveY
        printfn "Tx: %A, Ty: %A, TmoveX: %A, TmoveY: %A" Tx Ty TmoveX TmoveY
        moveRestOfTailMaybe (Tid, Tx+(HmoveX-TmoveX), Ty+(HmoveY-TmoveY-1), HmoveX, HmoveY, Tset.Add(Tx+HmoveX, Ty+HmoveY)) (Tl |> List.updateAt Tid (Tid, Tx+(HmoveX-TmoveX-1), Ty+(HmoveY-TmoveY-1), HmoveX, HmoveY, Tset.Add(Tx+HmoveX, Ty+HmoveY)))
    else Tl


let moveHeadFromInst (inst: Instruction) (Hh: H) (Tl: T list): H*T list =
    
    let rec aux (acc: H*T list) (inst: Instruction) : H*T list = 
        let H, Tl = acc
        let Hx, Hy, HmoveX, HmoveY, Hdir = H
        match inst with
        | dir, i when i > 0 ->
            match dir with
            | Direction.U -> aux ((Hx,Hy+1,HmoveX,HmoveY+1,dir),(moveTailIfNecessary (Hx,Hy+1,HmoveX,HmoveY+1,dir) Tl)) (dir,i-1)
            | Direction.R -> aux ((Hx+1,Hy,HmoveX+1,HmoveY,dir),(moveTailIfNecessary (Hx+1,Hy,HmoveX+1,HmoveY,dir) Tl)) (dir,i-1)
            | Direction.D -> aux ((Hx,Hy-1,HmoveX,HmoveY-1,dir),(moveTailIfNecessary (Hx,Hy-1,HmoveX,HmoveY-1,dir) Tl)) (dir,i-1)
            | Direction.L -> aux ((Hx-1,Hy,HmoveX-1,HmoveY,dir),(moveTailIfNecessary (Hx-1,Hy,HmoveX-1,HmoveY,dir) Tl)) (dir,i-1)
        | _ -> acc
    //needs to call move all tails if necessary with that input of head
    aux (Hh,Tl) inst


let giveHeadInstructions (arr: string list) =
    
    let getEnumFromString (str: string) : Direction =
        match str with
        | "U" -> Direction.U
        | "R" -> Direction.R
        | "D" -> Direction.D
        | "L" -> Direction.L
        | _   -> Direction.None // never happens
    
    let rec aux (acc: H*T list) (arr: string list): H*T list =
        let H, T = acc
        match arr with
        | [] -> acc
        | x :: xs -> aux (moveHeadFromInst (x.Split(" ")[0] |> getEnumFromString,x.Split(" ")[1] |> int) H T) xs
        
    aux ((0,0,0,0,Direction.None),(List.init 1 (fun id -> (id,0,0,0,0,Set.empty.Add(0,0))))) arr

let getTailVisits (HT: H*T list): int =
    let _, T = HT
    let _, _, _, _, _, Tset = (T |> List.last)
    Tset.Count


printfn "%A" (readLines "./Day_09/big.txt")