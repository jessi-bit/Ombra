module Ombra.Interpreter.Substitution

#load "interpreter-types.fs"
open Ombra.Interpreter.Types

#nowarn "25"

// -------------------------------------------------------------
// Ombra's interpreter - Lambda Calculus substitution semantics

let rec occursFree x N  =
    match N with
        | Lit y when y = x      -> true
        | Lam (y,_, e)          -> y <> x && occursFree x e
        | App (e1, e2)          -> occursFree x e1 || occursFree x e2
        | If (cond, ifE, elseE) -> occursFree x cond || occursFree x ifE || occursFree x elseE
        | _ -> false

let rec substitute M x N =
    match M with
        | Lit v when v = x        -> N
        | Lit _                   -> M
        | Lam (v,_, _) when v = x -> M
        | Lam (v, tp,e)           -> Lam (v, tp,substitute e x N)
        | App (e, e')             -> App ((substitute e x N), (substitute e' x N))
        | If (condE, ifE, elseE)  -> If ((substitute condE x N), (substitute ifE x N), (substitute elseE x N))
        | _                       -> M

let rec evalS = function
    | App (e, argE)          -> match (evalS e) with
                                    | Lam (var, _,  body) -> evalS (substitute body var argE)
    | If (condE, ifE, elseE) -> match evalS condE with
                                    | Bool true -> evalS ifE
                                    | _         ->  evalS elseE
    | e                      -> e