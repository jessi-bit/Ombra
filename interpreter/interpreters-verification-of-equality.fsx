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

//VarsSet
let varsSet= ref<Set<ident>> Set.empty

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
    | Lam (id, tp, body) ->
        let env' = Map.add id tp tEnv
        match (tpCheck env' body) with
            | Some t2 -> 
                FUN (tp, t2) |> Some
            | _ -> None

// Generator of closed terms -----------------------------------------------------

let goodId id =
    let vSet = varsSet.Value
    let ids = List.map string ['a'..'z'] 
    List.contains id ids && not (Set.contains id vSet)

let idGen =
    let vSet = varsSet.Value
    let letters = Seq.append ['a' .. 'z'] ['A' .. 'Z'] |> Seq.map string 
    let goodVars = seq {for i in letters do if not (Set.contains i vSet) then yield i}
    Gen.elements goodVars

// TODO generate a different var each time for lambda, use a global variable
// base case: bind between the range of var
// use lambda with types annotations
let rec generateExp size =
    match size with
        | 0 -> Gen.oneof [
            Gen.map Lit (Arb.generate<ident> |> Gen.filter goodId)
            Gen.map Bool Arb.generate<bool>]
        | n when n > 0 ->
            Gen.frequency [  
                (1, Gen.map Lit (Arb.generate<ident> |> Gen.filter goodId)); 
                (1, Gen.map Bool Arb.generate<bool>);
                (4, Gen.map3 (fun i tp e -> Lam (i, tp, e)) (Gen.map id (Arb.generate<ident> |> Gen.filter goodId)) (Gen.map id (Arb.generate<ty>)) (generateExp (size / 2)));
                (5, Gen.map2 (fun e e' -> App (e, e')) (generateExp (size / 2)) (generateExp (size / 2)))
            ]

let rec generateExp2 size =
    match size with
        | 0 -> Gen.oneof [
            Gen.map Lit idGen
            Gen.map Bool Arb.generate<bool>]
        | n when n > 0 ->
            Gen.frequency [  
                (1, Gen.map Lit idGen); 
                (1, Gen.map Bool Arb.generate<bool>);
                (3, Gen.map3 (fun i tp e -> Lam (i, tp, e)) (gen {let! id = idGen
                                                                  varsSet.Value<-Set.add id varsSet.Value
                                                                  return id}) 
                                                            (gen {let! tp = Arb.generate<ty>
                                                                  return tp}) 
                                                            (generateExp2 (size / 2)));
                (6, Gen.map2 (fun e e' -> App (e, e')) (generateExp2 (size / 2)) (generateExp2 (size / 2)));
                (6, Gen.map3 (fun cond e' e'' -> If (cond, e', e'')) (generateExp2 (size / 2)) (generateExp2 (size / 2)) (generateExp2 (size / 2)))
            ]

let rec verifyequality valueC valueS envC =
    match (valueC, valueS) with
        | Boo b1, BoolS b2 -> b1 = b2
        | Clos(id1, e1, _), LamS(id2, e2) -> 
            match (e1, e2) with
                | Lit x, Lit y -> x = y && id1 = id2  
                | _ ->
                    let e1' = evalC envC e1
                    let e2' = evalS e2
                    id1 = id2 && (verifyequality e1' e2' envC)

let propVal = 
            Gen.sized generateExp2 
            //seq {for e in (Gen.sized generateExp2) do if tpCheck Map.empty e <> None then yield e}
            //Gen.sized generateExp2
            // |> Gen.filter (fun exp ->
            //                match (tpCheck Map.empty exp) with
            //                    | Some _ -> true
            //                    | _ -> printf "\nFAILED\n%A\n\n" exp
            //                           false)
            |> Gen.filter (fun exp -> tpCheck Map.empty exp <> None)
            |> Arb.fromGen
            |> Prop.forAll <| fun (ast) ->
                let resS = evalS ast
                let resC = evalC Map.empty ast
                printf "\n********************\nAST was %A\nsubstitution: %A\nclosures: %A\n" ast resS resC
                verifyequality resC resS Map.empty

do Check.Quick propVal
