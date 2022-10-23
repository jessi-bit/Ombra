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

//TypeChecker -----------------------------------------------------------------
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

//Generator of closed terms -----------------------------------------------------
let areThereFreeVars exp =
    let rec loop exp varsSet =
        match exp with 
            | Lit x -> not (Set.contains x varsSet)
            | Bool _ -> false
            | Lam (var, body) as e -> 
                occursFree var e || loop body (Set.add var varsSet)
            | App (e1, e2) -> 
                loop e1 varsSet || loop e2 varsSet
            | If (e1,e2,e3) ->
                loop e1 varsSet || loop e2 varsSet || loop e3 varsSet

    loop exp (Set.empty)

areThereFreeVars (Lit "x") 
areThereFreeVars (Lam ("x", App (Lam ("x", Bool true), Bool false))) //false
areThereFreeVars (Lam ("x", App (Lam ("x", Lit "y"), Bool true))) //true
areThereFreeVars (App (Lam ("x", If (Lit "x", Lit "y", Bool false)), Bool true)) //true
areThereFreeVars (App (Lam ("x", If (Lit "x", Bool true, Bool false)), Bool true)) //false

let rec badExp = function
    | Lit ""          -> false
    | Lit null        -> false
    | Lam ("", _)     -> false
    | Lam (null, _)   -> false
    | Lam (_, e)      -> badExp e
    | App (e, e')     -> match e with
                             | Lam _ -> badExp e && badExp e'
                             | _     -> false
    | If (e, e', e'') -> match e with
                             | App _  -> badExp e && badExp e' && badExp e''
                             | Bool _ -> badExp e' && badExp e''
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
            Gen.frequency [ 
                (1, Gen.map Lit Arb.generate<ident>); 
                (1, Gen.map Bool Arb.generate<bool>);
                (4, Gen.map2 (fun i e -> Lam (i, e)) (Gen.map id Arb.generate<ident>) (generateExp (size - 1)));
                (4, Gen.map2 (fun e e' -> App (e, e')) (generateExp (size - 1)) (generateExp (size - 1)));
                (4,  Gen.map3 (fun e e' e'' -> If (e, e', e'')) (generateExp (size - 1)) (generateExp (size - 1)) (generateExp (size - 1)))
                ]
            
let rec verifyequality valueC valueS envC =
    match (valueC, valueS) with
        | Boo b1, BoolS b2 -> b1 = b2
        | Clos(id1, e1, _), LamS(id2, e2) -> 
            let e1' = evalC envC e1
            let e2' = evalS e2
            id1 = id2 && (verifyequality e1' e2' envC)
 
let lam = App (Lam ("x", Lit "x"), (App (Lam ("x", Lit "x"), Bool true)))
let res1 = evalS lam
let res2 = evalC Map.empty lam

verifyequality res2 res1 Map.empty

let propVal = Gen.sized generateExp
            |> Gen.filter badExp
            |> Gen.filter (fun exp -> not (areThereFreeVars exp))
            //|> Gen.filter (fun exp -> tpCheck Map.empty exp <> None)
            |> Arb.fromGen
            |> Prop.forAll <| fun (ast) ->
                let envC = fillEnv Map.empty List.empty ast
                let resS = evalS ast
                let resC = evalC envC ast
                printf "AST was  %A\nsubstitution: %A\nclosures: %A\n" ast resS resC
                verifyequality resC resS envC

do Check.Quick propVal

