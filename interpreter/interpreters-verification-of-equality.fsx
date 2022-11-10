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

let varsSet = ref<Set<ident>> Set.empty

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
    //TODO: the check that the var is not contained in the set in order to be generated
    //has just to be made for the lambda gen purpose, not for each Ident generation.
    let goodVars = seq {for i in letters do if not (Set.contains i vSet) then yield i}
    Gen.elements goodVars

// generation rules
let ruleBoolean () = Gen.map Bool Arb.generate<bool>
//TODO: Wrong. see the previous TODO.
let ruleIdent () = Gen.map Lit idGen
let ruleLambda generateExp size = Gen.map3 (fun i tp e -> Lam (i, tp, e))
                                        (gen {let! id = idGen
                                              varsSet.Value <- Set.add id varsSet.Value
                                              return id})
                                        (gen {let! tp = Arb.generate<ty>
                                              return tp})
                                        (generateExp (size / 2))
// the first argument of an App is always a Lam
let ruleApp generateExp size = Gen.map2 (fun e e' -> App (e, e')) (ruleLambda generateExp (size / 2)) (generateExp (size / 2))
let ruleAllButLambda generateExp size = Gen.oneof [
    ruleBoolean ()
    ruleIdent ()
    ruleApp generateExp (size / 2)]
let ruleIfe generateExp size = Gen.map3 (fun cond e' e'' -> If (cond, e', e'')) (ruleAllButLambda generateExp (size / 2)) (generateExp (size / 2)) (generateExp (size / 2))

let rec generateExp size =
    match size with
        | 0 -> Gen.oneof [ruleBoolean (); ruleIdent ()]
        | n when n > 0 ->
            Gen.oneof [
                ruleLambda generateExp (size / 2)
                ruleApp generateExp (size / 2)
                ruleIfe generateExp (size / 2)
            ]


let rec verifyEquality valueC valueS =

    let rec verifyEqualityInner eC eS =
        match (eC, eS) with
            | (Bool _, Bool _) -> true
            | (Lit l1, Lit l2) -> l1 = l2
            | (Lam (i1, _, e1), Lam (i2, _, e2)) when i1 = i2 -> verifyEqualityInner e1 e2
            | (App (eC', eC''), App (eS', eS'')) -> verifyEqualityInner eC' eS' && verifyEqualityInner eC'' eS''
            | (If (eCCond, eCThen, eCElse), If (eSCond, eSThen, eSElse)) -> verifyEqualityInner eCCond eSCond &&
                                                                                verifyEqualityInner eCThen eSThen &&
                                                                                verifyEqualityInner eCElse eSElse

    match (valueC, valueS) with
        | Boo bC, BoolS bS -> bC = bS
        | Clos(idC, eC, _), LamS(idS, eS) ->
            match (eC, eS) with
                | Lit x, Lit y -> x = y && idC = idS
                | _ ->
                    idC = idS && verifyEqualityInner eC eS

let propVal = 
            Gen.sized generateExp
            |> Gen.filter (fun exp -> tpCheck Map.empty exp <> None)
            |> Arb.fromGen
            |> Prop.forAll <| fun (ast) ->
                let resS = evalS ast
                let resC = evalC Map.empty ast
                printf "\n********************\nAST was %A\nsubstitution: %A\nclosures: %A\n" ast resS resC
                verifyEquality resC resS

do Check.Quick propVal
