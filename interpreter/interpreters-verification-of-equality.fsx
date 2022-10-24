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

// TypeChecker -----------------------------------------------------------------
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

let rec fillTpckEnv env = function
    | Lit l           -> Map.add l BOOL env
    | Lam (l, exp)    -> fillTpckEnv (Map.add l BOOL env) exp
    | App (e, e')     -> let env' = fillTpckEnv env e
                         fillTpckEnv env' e'
    | If (e, e', e'') -> let env' = fillTpckEnv env e
                         let env'' = fillTpckEnv env' e'
                         fillTpckEnv env'' e''
    | _ -> env

// Generator of closed terms -----------------------------------------------------
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

let goodId id =
    let ids = List.map string ['a'..'z']
    List.contains id ids

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
    // TODO remove
    if size > 5 then (Gen.map Bool Arb.generate<bool>)
    else
    match size with
        | 0 -> let gen = Gen.oneof [
                 Gen.map Lit (Arb.generate<ident> |> Gen.filter goodId)
                 Gen.map Bool Arb.generate<bool>
               ]
               gen
        | n when n > 0 ->
            let lambda size =
                Gen.map2 (fun i e -> Lam (i, e)) (Gen.map id Arb.generate<ident> |> Gen.filter goodId) (generateExp (size - 1))
            let app size =
                Gen.map2 (fun e e' -> App (e, e')) (lambda (size)) (generateExp (size - 1))
            let ifeBody size = Gen.oneof [
                Gen.map Lit (Arb.generate<ident> |> Gen.filter goodId);
                Gen.map Bool Arb.generate<bool>;
                app (size);
            ]
            let ife size =
                Gen.map3 (fun e e' e'' -> If (e, e', e'')) (ifeBody (size)) (generateExp (size - 1)) (generateExp (size - 1))
            Gen.frequency [(4, lambda (size)); (4, app (size)); (4, ife (size))]

let rec verifyequality valueC valueS envC =
    match (valueC, valueS) with
        | Boo b1, BoolS b2 -> b1 = b2
        | Clos(id1, e1, _), LamS(id2, e2) -> 
            let e1' = evalC envC e1
            let e2' = evalS e2
            id1 = id2 && (verifyequality e1' e2' envC)
 
let propVal = Gen.sized generateExp
            // |> Gen.filter (fun exp ->
            //                match tpCheck (fillTpckEnv Map.empty exp) exp with
            //                    | Some _ -> true
            //                    | _ -> printf "\nFAILED\n%A\n\n" exp
            //                           false)
            |> Gen.filter (fun exp -> not (areThereFreeVars exp))
            //|> Gen.filter (fun exp -> tpCheck Map.empty exp <> None)
            |> Arb.fromGen
            |> Prop.forAll <| fun (ast) ->
                let envC = fillEnv Map.empty List.empty ast
                let resS = evalS ast
                let resC = evalC envC ast
                printf "\n********************\nAST was %A\nsubstitution: %A\nclosures: %A\n" ast resS resC
                verifyequality resC resS envC

let r = Gen.sized generateExp |>
            Gen.filter (fun exp -> not (areThereFreeVars exp)) |>
            Gen.filter (fun exp -> match tpCheck (fillTpckEnv Map.empty exp) exp with
                                       | Some _ -> true
                                       | None   -> printf "\n\n**************** DID NOT TPCK\n %A\n\n\n" exp
                                                   false) |>
            Gen.filter (fun exp -> match exp with
                                       | Bool _ -> false
                                       | e -> true) |>
            Gen.sample 1 10 |>
            List.forall (fun (ast) ->
                let envC = fillEnv Map.empty List.empty ast
                let resS = evalS ast
                let resC = evalC envC ast
                printf "\n********************\nAST was %A\nsubstitution: %A\nclosures: %A\n" ast resS resC
                verifyequality resC resS envC)

printf "%A\n" r

// do Check.Quick propVal
