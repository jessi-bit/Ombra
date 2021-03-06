module Ombra.Ops 

open Ombra.Types 

// ---------------------------------------------
// Operations on ints

let mapInt funct exp1 exp2 =
    match exp1, exp2 with
        | Atom (K k), Atom (K k2) -> Atom (K (funct k k2))
        | Atom (K k), Atom Nul -> Atom (K k)
        | _-> Atom Nul

let intOp elements funx expFun = 
    match elements with
        | [] -> Atom Nul
        | [Atom (K _) as atom] -> atom
        | head :: tail ->
            mapInt funx head (expFun tail)

let rec plus elements =
    intOp elements (+) plus
let rec mul elements =
    intOp elements (*) mul
let rec minus elements =
    intOp elements (-) minus

let cmp elements funct =
    match elements with
        | [Atom (K k); Atom (K k1)] -> Atom (B (funct k k1))
        | _ -> Atom Nul

let greater elements =
    cmp elements (>)

let lesser elements =
    cmp elements (<)

// ---------------------------------------------
// Operations on bools

let mapBool funct exp1 exp2 =
    match exp1, exp2 with
        | Atom (B b1), Atom (B b2) -> Atom (B (funct b1 b2))
        | Atom (B b), Atom Nul -> Atom (B b)
        | _-> Atom Nul

let boolOp elements funx expFun =
    match elements with
        | [] -> Atom Nul
        | [Atom (B _) as atom] -> atom
        | head :: tail ->
            mapBool funx head (expFun tail)

let rec andB elements =
    boolOp elements (&&) andB
let rec orB elements =
    boolOp elements (||) andB

let notB elements =
    match elements with
        | [Atom (B b)] -> Atom (B (not b))
        | _ -> Atom Nul

// ---------------------------------------------
// Operations on strings

let mapSt funct exp1 exp2 =
    match exp1, exp2 with
        | Atom (S s), Atom (S s2) -> Atom (S (funct s s2))
        | Atom (S s), Atom Nul -> Atom (S s)
        | _-> Atom Nul

let strOp elements funx expFun =
    match elements with
        | [] -> Atom Nul
        | [Atom (S _) as atom] -> atom
        | head :: tail ->
            mapSt funx head (expFun tail)

let rec cat elements =
    strOp elements (+) cat


// ---------------------------------------------
// Operations on lists

let quote elements =
    match elements with
        | [SubExp l] -> 
            List l
        | _ -> Atom Nul

let cons elements =
    match elements with
        | (Atom _ as head) :: tail ->
            match tail with
                | [List els] -> List (head :: els)
                | _ -> Atom Nul
        | _ -> Atom Nul

let car elements = 
    match elements with
        | [List (head :: _)] -> head
        | _ -> Atom Nul

let cdr elements =
    match elements with
        | [List (_ :: tail)] -> List tail
        | _ -> Atom Nul
let caar elements =
    match elements with
        | [List (_ :: head2 :: _)] -> head2
        | _ -> Atom Nul

//type checker -> len is ok for list and strings
let len elements =
    match elements with 
        | [List ls] -> Atom (K (List.length ls))
        | [Atom (S s)] -> Atom (K (String.length s))
        | _ -> Atom Nul

//Polymorfic operation
let rec isEqual exp1 exp2 =
    match (exp1, exp2) with
        | Atom (K k), Atom (K k1) -> k = k1
        | Atom (S s), Atom (S s1) -> s = s1
        | Atom (B b), Atom (B b1) -> b = b1
        | Atom (Var v), Atom (Var v1) -> v = v1
        | Op s, Op s1 -> s = s1
        | List l1, List l2 -> areEquals l1 l2
        | _-> false

and areEquals xs ys =
    match (xs, ys) with
        | [], [] -> true
        | head1 :: tail1, head2 :: tail2 -> 
            isEqual head1 head2 && (areEquals tail1 tail2)
        | _ -> false

let eq elements =
    match elements with
        | [fst; snd] -> Atom (B (isEqual fst snd))
        | _ -> Atom Nul

let symbTable =
            Map.empty
                .Add("+", plus)
                .Add("*", mul) 
                .Add("-", minus)
                .Add("'", quote)
                .Add("cons", cons)
                .Add("car", car)
                .Add("cdr", cdr)
                .Add("caar", caar)
                .Add("and", andB)
                .Add("or", orB)
                .Add("append", cat)
                .Add("length", len)
                .Add("not", notB)
                .Add(">", greater)
                .Add("<", lesser)
                .Add("=", eq)