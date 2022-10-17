module Ombra.Interpreter.Substitution

open Ombra.Interpreter.Types

// -------------------------------------------------------------
// Ombra's interpreter - Lambda Calculus substitution semantics

type valueS =
    | BoolS of bool
    | LamS of (ident * exp)

let rec occursFree x N  =
    match N with
        | Lit y when y = x      -> true
        | Lam (y, e)            -> y <> x && occursFree x e
        | App (e1, e2)          -> occursFree x e1 || occursFree x e2
        | If (cond, ifE, elseE) -> occursFree x cond || occursFree x ifE || occursFree x elseE
        | _ -> false

let rec substitute M x N =
    match M with
        | Lit v when v = x       -> N
        | Lit _                  -> M
        | Lam (v, _) when v = x  -> M
        | Lam (v, e)             -> Lam (v, substitute e x N)
        | App (e, e')            -> App ((substitute e x N), (substitute e' x N))
        | If (condE, ifE, elseE) -> If ((substitute condE x N), (substitute ifE x N), (substitute elseE x N))
        | _                      -> M

let rec evalS = function
    | Bool b                 -> BoolS b
    | Lit _                  -> failwith "literal not a value"
    | Lam e                  -> LamS e
    | App (e, argE)          -> match (evalS e) with
                                    | LamS (var, body) -> evalS (substitute body var argE)
    | If (condE, ifE, elseE) -> match evalS condE with
                                    | BoolS true -> evalS ifE
                                    | _          ->  evalS elseE

// -------------------------------------------------------------
// Ombra's interpreter - Tests

let lam = App (Lam ("x", Lit "x"), (App (Lam ("x", Lit "x"), Bool true)))
evalS lam

let idE = Lam ("x", Lit "x")
let appInAppInApp = App (App (App (idE, idE), idE), Bool false)
evalS appInAppInApp

// K combinator that returns false
let cond  = App (Lam ("x", Bool false), Bool true)
let ifE   = Bool true
let elseE = Bool false
evalS (If (cond, ifE, elseE)) // false
