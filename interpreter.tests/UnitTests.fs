module Ombra.InterpreterTest

open NUnit.Framework
open Ombra.Interpreter
//open FsCheck

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestConfig () =
    Assert.Pass()

[<Test>]
let TestSum () =
    // (+ 1 41)
    let exp = Function [Symb "+"; Atom (K 1); Atom (K 41)]
    let env = (E Map.empty)
    match (eval exp env) with
        | Atom (K k) -> Assert.AreEqual (42, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestSub () =
    // (- 43 1)
    let exp = Function [Symb "-"; Atom (K 43); Atom (K 1)]
    let env = (E Map.empty)
    match (eval exp env) with
        | Atom (K k) -> Assert.AreEqual (42, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestSumWithVars () =
    // env: x = 2, y = 2
    // (+ x y 5)
    let exp = Function [Symb "+"; Var "x"; Var "y"; Atom (K 5)]
    let env = (E (Map.add "x" (Atom (K 2)) (Map.add "y" (Atom (K 2)) Map.empty)))
    match (eval exp env) with
        | Atom (K k) -> Assert.AreEqual (9, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestMul () =
    // (* 5 5)
    let exp = Function [Symb "*"; Atom (K 5); Atom (K 5)]
    let env = E (Map.empty)
    match (eval exp env) with
        | Atom (K k) -> Assert.AreEqual (25, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLambdaBase1 () =
    // (lambda () 41)
    let aLambda = Lambda ([], Atom (K 41), [])
    let env = (E Map.empty)
    match (eval aLambda env) with
        | Atom (K k) -> Assert.AreEqual (41, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLambdaBase2 () =
    // (+ 1 ((lambda (x) x) 41))
    let anotherL = Lambda (["x"], Var "x", [Atom (K 41)])
    let f = Function [Symb "+"; (Atom (K 1)); anotherL]
    let env = (E Map.empty)
    match (eval f env) with
        | Atom (K k) -> Assert.AreEqual (42, k)
        | _ -> Assert.Fail()
    Assert.Pass()

    // (+ 1 ((lambda (x) x) 41))

[<Test>]
let TestLambdaComplex () =
    // ((lambda (x y) (+ x y)) 1 2)
    let lambdaFun = Function[Symb "+"; Var "x"; Var "y"]
    let jLambda = Lambda (["x"; "y"], lambdaFun, [Atom(K 1); Atom (K 2)])
    let env = (E Map.empty)
    match (eval jLambda env) with
        | Atom (K k) -> Assert.AreEqual (3, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLambdaComplex2 () =
    // ((lambda (x y) (+ x y)) 1 ((lambda ((x y) (+ x y)) 2 3)))
    let f1 = Function[Symb "+"; Var "x"; Var "y"]
    let lambdaParm = Lambda (["x"; "y"], f1, [Atom (K 2); Atom (K 3)])
    let mainLambda = Lambda (["x"; "y"], f1, [Atom (K 1); lambdaParm])
    let env = E Map.empty
    match (eval mainLambda env) with
        | Atom (K k) -> Assert.AreEqual (6, k)
        | _ -> Assert.Fail()
    Assert.Pass()

let isEqual exp1 exp2 =
    match (exp1, exp2) with
        | Atom (K k), Atom (K k1) -> k = k1
        | Atom (S s), Atom (S s1) -> s = s1
        | Atom (B b), Atom (B b1) -> b = b1
        | Var v, Var v1 -> v = v1
        | Symb s, Symb s1 -> s = s1
        | _-> false

let rec areEquals xs ys =
    match (xs, ys) with
        | [], [] -> true
        | head1 :: tail1, head2 :: tail2 -> 
            isEqual head1 head2 && (areEquals tail1 tail2)
        | _ -> false

[<Test>]
let TestQuote () =
    // (quote (1 2 3))  
    let quote = Quote (List[Atom (K 1); Atom (K 2); Atom (K 3)])
    let env = E Map.empty
    match (eval quote env) with
        | List lst -> Assert.AreEqual (areEquals lst ([Atom (K 1); Atom (K 2); Atom (K 3)]), true)
        | _ -> Assert.Fail()
    Assert.Pass()

