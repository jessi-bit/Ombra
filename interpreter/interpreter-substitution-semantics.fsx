module Ombra.Interpreter.Substitution

open Ombra.Interpreter.Types

// -------------------------------------------------------------
// Ombra's interpreter - Lambda Calculus substitution semantics

// alpha-conversion and beta-reduction are explained here
// https://pages.cs.wisc.edu/~horwitz/CS704-NOTES/1.LAMBDA-CALCULUS.html#beta
// we chose not to go with De Bruijn indexes

// precondition: z does not occur in M
// returns M with all free occurrences of x replaced by z
let rec α M x z =
    match M with
        | Lit v when v = x      -> Lit z
        | Lit _ | Bool _        -> M
        | Lam (v, _) when v = x -> M
        | Lam (v, e)            -> Lam (v, α e x z)
        | App (e, e')           -> App (α e x z, α e' x z)
        | If (cond, ifE, elseE) -> If (α cond x z, α ifE x z, α elseE x z)

// (λx.M)N → β M[N/x]
// "[...] the notation means M with all free occurrences of x replaced with N
// in a way that avoids capture. We say that (λx.M)N beta-reduces to M with N
// substituted for x."
let rec occursFree x N  =
    match N with
        | Lit y when y = x      -> true
        | Lam (y, e)            -> y <> x && occursFree x e
        | App (e1, e2)          -> occursFree x e1 || occursFree x e2
        | If (cond, ifE, elseE) -> occursFree x cond || occursFree x ifE || occursFree x elseE
        | _ -> false

let identsSet e = 
    let rec idents e = 
        match e with 
            | Lit ident        -> [ident]
            | Lam (ident, exp) -> ident :: (idents exp)
            | App (e, e')      -> (idents e) @ (idents e')
            | If (e, e', e'')  -> (idents e) @ (idents e') @ (idents e'')
            | _                -> []
    idents e |> Set.ofList

let chooseIdent x y N e =
    let vars = Seq.initInfinite (fun num -> "X" + string num)
    let filtered usedVarsSet = Seq.filter (fun var -> not (Set.contains var usedVarsSet)) vars |> Seq.cache
    let varsSet = Set.union (identsSet N) (Set.union (identsSet e) (Set.add y (Set.add x Set.empty)))
    Seq.item 0 (filtered varsSet) 

let rec β M x N =
    match M with
        | Lit v when v = x                -> N
        | Lit _                           -> M
        | Lam (v, _) when v = x           -> M
        | Lam (v, e) when occursFree v N  ->
            let v' = chooseIdent x v N e
            let e' = α e v v'                    
            Lam (v', β e' x N)
        | Lam (v, e)                      -> Lam (v, β e x N)
        | App (e, e')                     -> App ((β e x N), (β e' x N))
        // --- outside Lambda Calculus
        | If (condE, ifE, elseE)          -> If ((β condE x N), (β ifE x N), (β elseE x N))
        | _ -> M

let rec evalS = function
    | App (lam, argE)        -> let arg = evalS argE
                                match (evalS lam) with
                                    | Lam (var, body) -> evalS (β body var arg)
    | If (condE, ifE, elseE) -> match evalS condE with
                                    | Bool true -> ifE
                                    | _         -> elseE
    | e -> e

// -------------------------------------------------------------
// Ombra's interpreter - Tests

// K combinator that returns false
let cond  = App (Lam ("x", Bool false), Bool true)
let ifE   = Bool true
let elseE = Bool false
evalS (If (cond, ifE, elseE)) // false
