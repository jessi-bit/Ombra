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
    let exp = List [Symbol "+"; Value (K 1); Value (K 41)]
    let env = (E Map.empty)
    match (eval exp env) with
        | Value (K k) -> Assert.AreEqual (42, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestSub () =
    // (- 43 1)
    let exp = List [Symbol "-"; Value (K 43); Value (K 1)]
    let env = (E Map.empty)
    match (eval exp env) with
        | Value (K k) -> Assert.AreEqual (42, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestSumWithVars () =
    // env: x = 2, y = 2
    // (+ x y 5)
    let exp = List [Symbol "+"; Var (V "x"); Var (V "y"); Value (K 5)]
    let env = (E (Map.add (V "x") (K 2) (Map.add (V "y") (K 2) Map.empty)))
    match (eval exp env) with
        | Value (K k) -> Assert.AreEqual (9, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestMul () =
    // (* 5 5)
    let exp = List [Symbol "*"; Value (K 5); Value (K 5)]
    let env = E (Map.empty)
    match (eval exp env) with
        | Value (K k) -> Assert.AreEqual (25, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLambdaBase1 () =
    // (+ 1 ((lambda () 41)))
    let invokedLambda = List [List [Symbol "lambda"; List []; Value (K 41)]]
    let astUsingLambda = List [Symbol "+"; (Value (K 1)); invokedLambda]
    let env = (E Map.empty)
    match (eval astUsingLambda env) with
        | Value (K k) -> Assert.AreEqual (42, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLambdaBase2 () =
    // (+ 1 ((lambda (x) x) 41))
    let invokedLambda2 = List [List [Symbol "lambda"; List [Var (V "x")]; Var (V "x")]; (Value (K 41))]
    let astUsingLambda2 = List [Symbol "+"; (Value (K 1)); invokedLambda2]
    let env = (E Map.empty)
    match (eval astUsingLambda2 env) with
        | Value (K k) -> Assert.AreEqual (42, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLambdaComplex () =
    // ((lambda (x y) (+ x y)) 1 2)
    let parms = List [Var (V "x"); Var (V "y")]
    let body = List [Symbol "+"; Var (V "x"); Var (V "y")]
    let jLambda = List [List [Symbol "lambda"; parms ; body]; Value (K 1); Value(K 2)]
    let env = (E Map.empty)
    match (eval jLambda env) with
        | Value (K k) -> Assert.AreEqual (3, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLambdaComplex2 () =
    //((lambda (x y) (+ x y)) 1 ((lambda ((x y) (+ x y)) 2 3)))
    let body = List [Symbol "+"; Var (V "x"); Var (V "y")]
    let parms = List [Var (V "x"); Var (V "y")]
    let paramLambda = List [List [Symbol "lambda"; parms; body]; Value (K 2); Value (K 3)]
    let jLambda = List [List [Symbol "lambda"; parms ; body]; Value (K 1); paramLambda]
    let env = E Map.empty
    match (eval jLambda env) with
        | Value (K k) -> Assert.AreEqual (6, k)
        | _ -> Assert.Fail()
    Assert.Pass()

let rec areEqual (List xs) (List ys) =
    match (xs, ys) with
        | [], [] -> true
        | Value (K k) :: tail1, Value (K k1) :: tail2 -> 
            k = k1 && (areEqual (List tail1) (List tail2))

[<Test>]
let TestQuote () =
    // (quote (1 2 3))
    let quote = List [Symbol "quote"; List [Value (K 1); Value (K 2); Value (K 3)]]
    let env = E Map.empty
    match (eval quote env) with
        | lst -> Assert.AreEqual (areEqual lst (List [Value (K 1); Value (K 2); Value (K 3)]), true)
        | _ -> Assert.Fail()
    Assert.Pass()
