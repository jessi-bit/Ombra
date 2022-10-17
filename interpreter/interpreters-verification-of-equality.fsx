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

// "typechecker"
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

let cases = Gen.sized generateExp
            |> Gen.filter tc
            |> Arb.fromGen
            |> Prop.forAll <| fun (ast) ->
                let resS = evalS ast
                let resC = evalC (fillEnv Map.empty List.empty ast) ast
                printf "AST was  %A\nsubstitution: %A\nclosures: %A\n" ast resS resC
                true

Check.Quick cases