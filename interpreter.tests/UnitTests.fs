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
    let env = (E (Map.add "x" (K 2) (Map.add "y" (K 2) Map.empty)))
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

// [<Test>]
// let TestLambdaBase1 () =
//     // (+ 1 ((lambda () 41)))
//     let invokedLambda = List [List [Symbol "lambda"; List []; Value (K 41)]]
//     let astUsingLambda = List [Symbol "+"; (Value (K 1)); invokedLambda]
//     let env = (E Map.empty)
//     match (eval astUsingLambda env) with
//         | Value (K k) -> Assert.AreEqual (42, k)
//         | _ -> Assert.Fail()
//     Assert.Pass()

// [<Test>]
// let TestLambdaBase2 () =
//     // (+ 1((lambda (x) x) 41))
//     let invokedLambda2 = List [List [Symbol "lambda"; List [Var (V "x")]; Var (V "x")]; (Value (K 41))]
//     let astUsingLambda2 = List [Symbol "+"; (Value (K 1)); invokedLambda2]
//     let env = (E Map.empty)
//     match (eval astUsingLambda2 env) with
//         | Value (K k) -> Assert.AreEqual (42, k)
//         | _ -> Assert.Fail()
//     Assert.Pass()

// [<Test>]
// let TestLambdaComplex () =
//     // ((lambda (x y) (+ x y)) 1 2)
//     let parms = List [Var (V "x"); Var (V "y")]
//     let body = List [Symbol "+"; Var (V "x"); Var (V "y")]
//     let jLambda = List [List [Symbol "lambda"; parms ; body]; Value (K 1); Value(K 2)]
//     let env = (E Map.empty)
//     match (eval jLambda env) with
//         | Value (K k) -> Assert.AreEqual (3, k)
//         | _ -> Assert.Fail()
//     Assert.Pass()

// [<Test>]
// let TestLambdaComplex2 () =
//     //((lambda (x y) (+ x y)) 1 ((lambda ((x y) (+ x y)) 2 3)))
    
//     let body = List [Symbol "+"; Var (V "x"); Var (V "y")]
//     let parms = List [Var (V "x"); Var (V "y")]
//     let paramLambda = List [List [Symbol "lambda"; parms; body]; Value (K 2); Value (K 3)]
//     let jLambda = List [List [Symbol "lambda"; parms ; body]; Value (K 1); paramLambda]
//     let env = E Map.empty
//     match (eval jLambda env) with
//         | Value (K k) -> Assert.AreEqual (6, k)
//         | _ -> Assert.Fail()
//     Assert.Pass()

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
    let quote = Function[Symb "quote"; List[Atom (K 1); Atom (K 2); Atom (K 3)]]
    let env = E Map.empty
    match (eval quote env) with
        | List lst -> Assert.AreEqual (areEquals lst ([Atom (K 1); Atom (K 2); Atom (K 3)]), true)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestQuoteFunc () =
    // (quote (+ 2 x))  
    let quote = Function[Symb "quote"; Function[Symb "+"; Atom (K 2); Var "x"]]
    let env = E Map.empty
    match (eval quote env) with
        | Function lst -> Assert.AreEqual (areEquals lst ([Symb "+"; Atom (K 2); Var "x"]), true)
        | _ -> Assert.Fail()
    Assert.Pass()
