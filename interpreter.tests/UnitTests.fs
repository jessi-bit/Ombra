module interpreter.tests

open NUnit.Framework
open AST
//open FsCheck

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestConfig () =
    Assert.Pass()

[<Test>]
let TestSum () =
    let exp = List [Symbol "+"; Value (K 1); Value (K 41)]
    let env = emptyEnv
    match (eval exp env) with
        | Value (K k) -> Assert.AreEqual (42, k)
        | _ -> Assert.Fail()
    Assert.Pass()

// [<Test>]
// let TestSumWithVars () =
//     let exp = List [Symbol "+"; Var (V "x"); Var (V "y"); Value (K 5)]
//     let env = (E (Map.add (V "x") (K 2) (Map.add (V "y") (K 2) Map.empty)))
//     match (eval exp env) with
//         | Value (K k) -> Assert.AreEqual (4, k)
//         | _ -> Assert.Fail()
//     Assert.Pass()

// [<Test>]
// let TestLambdaBase1 () =
//     let invokedLambda = List [List [Symbol "lambda"; List []; Value (K 41)]]
//     let astUsingLambda = List [Symbol "+"; (Value (K 1)); invokedLambda]
//     let env = emptyEnv
//     match (eval astUsingLambda env) with
//         | Value (K k) -> Assert.AreEqual (42, k)
//         | _ -> Assert.Fail()
//     Assert.Pass()

// [<Test>]
// let TestLambdaBase2 () =
//     let invokedLambda2 = List [List [Symbol "lambda"; List [Var (V "x")]; Var (V "x")]; (Value (K 41))]
//     let astUsingLambda2 = List [Symbol "+"; (Value (K 1)); invokedLambda2]
//     let env = emptyEnv
//     match (eval invokedLambda2 env) with
//         | Value (K k) -> Assert.AreEqual (42, k)
//         | _ -> Assert.Fail()
//     Assert.Pass()

// [<Test>]
// let TestLambdaComplex () =
//     let parms = List [Var (V "x"); Var (V "y")]
//     let body = List [Symbol "+"; Var (V "x"); Var (V "y")]
//     let jLambda = List [List [Symbol "lambda"; parms ; body]; Value (K 1); Value(K 2)]
//     let env = emptyEnv
//     match (eval jLambda env) with
//         | Value (K k) -> Assert.AreEqual (3, k)
//         | _ -> Assert.Fail()
//     Assert.Pass()
