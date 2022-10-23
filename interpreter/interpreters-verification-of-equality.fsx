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

let rec tpCheck tEnv = function
    | Lit x -> Map.tryFind x tEnv
    | Bool _ -> Some BOOL
    | If (e1,e2,e3) ->
        match (tpCheck tEnv e1, tpCheck tEnv e2, tpCheck tEnv e3) with
            | Some BOOL, Some t1, Some t2 when t1 = t2 -> Some t1
            | _ -> None
    | App (e1, e2) ->
        match (tpCheck tEnv e1, tpCheck tEnv e2) with
            | Some (FUN (t1, t2)), Some t3 when t3 = t1 -> Some t2
            | _ -> None
    | Lam (id, body) -> 
        match (Map.tryFind id tEnv, tpCheck tEnv body) with
            | Some t1, Some t2 -> FUN (t1, t2) |> Some
            | _ -> None
    
let lambda = Lam ("x", If (Lit "x", Bool true, Bool false))   
tpCheck (Map.add "x" BOOL Map.empty) lambda

let app = App (lambda, Bool true)
tpCheck (Map.add "x" BOOL Map.empty) app 

//filter of expressions having no meaning
let rec filterBad = function
    | Lit ""          -> false
    | Lit null        -> false
    | Lam ("", _)     -> false
    | Lam (null, _)   -> false
    | Lam (_, e)      -> filterBad e
    | App (e, e')     -> match e with
                             | Lam _ -> filterBad e && filterBad e'
                             | _     -> false
    | If (e, e', e'') -> match e with
                             | App _  -> filterBad e && filterBad e' && filterBad e''
                             | Bool _ -> filterBad e' && filterBad e''
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

let cases = Gen.sized generateExp
            |> Gen.filter filterBad
            |> Arb.fromGen
            |> Prop.forAll <| fun (ast) ->
                let resS = evalS ast
                let resC = evalC (fillEnv Map.empty List.empty ast) ast
                printf "AST was  %A\nsubstitution: %A\nclosures: %A\n" ast resS resC
                true

Check.Quick cases