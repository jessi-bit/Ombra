module Ombra.Interpreter.Verification

#nowarn "25"

#r "FsCheck"
open FsCheck
#load "interpreter-types.fs"
open Ombra.Interpreter.Types
#load "interpreter-closures-semantics.fsx"
open Ombra.Interpreter.Closures
#load "interpreter-substitution-semantics.fsx"
open Ombra.Interpreter.Substitution

// typechecker
let rec tc = function
    | Lit ""          -> false
    | Lit null        -> false
    | Lam ("", _)     -> false
    | Lam (null, _)   -> false
    | Lam (_, e)      -> tc e
    | App (e, e')     -> match e with
                             | Lam _ -> tc e && tc e'
                             | _     -> false
    | If (e, e', e'') -> match e with
                             | App _  -> tc e && tc e' && tc e''
                             | Bool _ -> tc e' && tc e''
                             | _      -> false
    | _               -> true

let rec fillEnv env bound = function
    | Lit l -> if not (List.contains l bound) then Map.add l (Boo true) env
               else env
    | Lam (l, exp) -> let env' = Map.add l (Boo true) env
                      let bound' = l :: bound
                      fillEnv env' bound' exp
    | App (e, e') -> let env' = fillEnv env bound e
                     fillEnv env' bound e'
    | If (e, e', e'') -> let env' = fillEnv env bound e
                         let env'' = fillEnv env' bound e'
                         fillEnv env'' bound e''
    | _ -> env

let rec generateExp size =
    match size with
        | 0 -> Gen.oneof [
            Gen.map Lit Arb.generate<ident>
            Gen.map Bool Arb.generate<bool>]
        | n when n > 0 ->
            Gen.oneof [
                Gen.map Lit Arb.generate<ident>
                Gen.map Bool Arb.generate<bool>
                Gen.map2 (fun i e -> Lam (i, e)) (Gen.map (fun i -> i) Arb.generate<ident>) (generateExp (size - 1))
                Gen.map2 (fun e e' -> App (e, e')) (generateExp (size - 1)) (generateExp (size - 1))
                Gen.map3 (fun e e' e'' -> If (e, e', e'')) (generateExp (size - 1)) (generateExp (size - 1)) (generateExp (size - 1))]

// let sized = Gen.sized generateExp
// let samples = Gen.sample 4 4 sized

// TODO expand with other cases
let rec equalityCheck e e' =
    match (e, e') with
        | (Bool b, Boo b') when b = b' -> true
        | _ -> false

// TODO filter using tc when generating, not after generation
let cases = Gen.sized generateExp
            |> Gen.filter tc
            |> Arb.fromGen
            |> Prop.forAll <| fun (ast) ->
                printf "DOING %A\n" ast

                let evaled = evalS ast
                let evaled' = evalC (fillEnv Map.empty List.empty ast) ast

                // this is because we need to fill the env
                // with data, because FsCheck generates Lit(s) that
                // might not be in the env
                let evaledS = match evaled with
                                  | Lit _ -> Bool true
                                  | _ -> evaled

                equalityCheck evaledS evaled'

Check.Quick cases



let ast = App (Lam ("y", (Lam ("x", Lit "y"))), Lit "x")
evalS ast
evalC (Map.add "x" (Boo true) Map.empty) ast


// EVAL SUBSTITUTION
// Lam ("X0", Lit "x")

// EVAL CLOSURES
// Clos ("x", Lit "y", map [("x", Boo true); ("y", Boo true)])
