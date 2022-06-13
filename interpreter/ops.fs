module Ombra.Ops 

open Ombra.Types 

let mapInt funct exp1 exp2 =
    match exp1, exp2 with
        | Atom (K k), Atom (K k2) -> Atom (K (funct k k2))
        | Atom (K k), Atom None -> Atom (K k)
        | _-> Atom None

let intOp elements funx expFun = 
    match elements with
        | [] -> Atom None
        | [Atom (K _) as atom] -> atom
        | head :: tail ->
            mapInt funx head (expFun tail)

let rec plus elements =
    intOp elements (+) plus
let rec mul elements =
    intOp elements (*) mul
let rec minus elements =
    intOp elements (-) minus

let rec quote elements =
    match elements with
        | [List l] -> 
            match (List.head l) with
                | Op _ -> SubExp l
                | _ -> List l
        | _ -> Atom None

let rec cons elements =
    match elements with
        | (Atom _ as head) :: tail ->
            match tail with
                | [Atom Nil] -> List (head :: [Atom Nil])
                | [List els] -> List (head :: els)
                | _ -> Atom None
        | _ -> Atom None