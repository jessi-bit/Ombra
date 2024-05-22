module Ombra.Interpreter.FixedVerification

#nowarn "25"

open System
#load "interpreter-types.fs"
open Ombra.Interpreter.Types
#load "interpreter-closures-semantics.fsx"
open Ombra.Interpreter.Closures
#load "interpreter-substitution-semantics.fsx"
open Ombra.Interpreter.Substitution


let rec verifyEquality valueC valueS =

    let rec verifyEqualityInner eC eS =
        match (eC, eS) with
            | (Bool _, Bool _) -> true
            | (Lit l1, Lit l2) -> l1 = l2
            | (Lam (i1, _, e1), Lam (i2, _, e2)) when i1 = i2 -> verifyEqualityInner e1 e2
            | (App (eC', eC''), App (eS', eS'')) -> verifyEqualityInner eC' eS' && verifyEqualityInner eC'' eS''
            | (If (eCCond, eCThen, eCElse), If (eSCond, eSThen, eSElse)) ->
                verifyEqualityInner eCCond eSCond && verifyEqualityInner eCThen eSThen && verifyEqualityInner eCElse eSElse

    match (valueC, valueS) with
        | Boo bC, Bool bS -> bC = bS
        | Clos(idC, eC, _), Lam (idS, _, eS) ->
            match (eC, eS) with
                | Lit x, Lit y -> x = y && idC = idS
                | _ ->
                    idC = idS && verifyEqualityInner eC eS

let rec toCode ast indentation =
    let ind () = String.replicate indentation "  "

    let handleIf (eCond, eThen, eElse) =
        let rCond = match eCond with
                        | (Bool b) -> String.Format("{0}", b.ToString())
                        | (Lit l)  -> String.Format("{0}", l.ToString())
        let rThen = toCode eThen (indentation + 1)
        let rElse = toCode eElse (indentation + 1)
        String.Format("{0}if {1}\n{2}\n{3}else\n{4}", ind (), rCond, rThen, ind (), rElse)

    let handleApp e =
        match e with
            | (e, Bool b) -> String.Format("{0}({1})({2})", ind (), toCode e 0, b.ToString())
            | (Bool b, e) -> String.Format("({0})({1})", b.ToString(), toCode e 0)
            | (e', e'')   -> String.Format("({0})({1})", toCode e' indentation, toCode e'' indentation)

    let handleLam (ident, _, e) =
        match e with
            | Bool b -> String.Format("fun {0} {1}", ident, toCode e indentation)
            | If i   -> String.Format("fun {0} \n{1}", ident, toCode e (indentation + 1))
            | App a  -> handleApp a

    match ast with
        | Bool b -> String.Format("{0}{1}", ind (), b.ToString())
        | Lit l  -> String.Format("{0}{1}", ind (), l.ToString())
        | If i   -> handleIf i
        | Lam l  -> handleLam l
        | App a  -> handleApp a

let ast0 = App (Lam ("c", BOOL, If (Bool false, Bool false, Bool true)), Bool true)
let ast1 = If (Bool true, App (Lam ("m", BOOL, Bool false), Bool false), Bool true)
let ast2 = Lam ("z", FUN (BOOL, FUN (BOOL, FUN (BOOL, BOOL))), If (Bool true, Bool true, Bool true))
let ast3 = Lam ("v", FUN (FUN (BOOL, FUN (BOOL, FUN (FUN (BOOL, BOOL), BOOL))), BOOL), If (Bool true, App (Lam ("U", BOOL, Bool true), Bool true), App (Lam ("G", BOOL, Bool false), Bool false)))

let shuffleG xs = xs |> Seq.sortBy (fun _ -> Guid.NewGuid())


let verify () =
    let ast = [ast0;ast1;ast2;ast3] |> shuffleG |> Seq.head
    let resS = evalS ast
    let resC = evalC Map.empty ast
    [sprintf "%A" ast; sprintf "%A" resS; sprintf "%A" resC; sprintf "%A" (verifyEquality resC resS); toCode ast 0]

// this is meant to be called as a library
// verify ()
