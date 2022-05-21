module interpreter.tests

open AST

open NUnit.Framework
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
    match (eval env exp) with
        | Value (K k) -> Assert.AreEqual (42, k)
        | _ -> Assert.Fail()
    Assert.Pass()

[<Test>]
let TestLambdaBase1 () =
    let invokedLambda = List [List [Symbol "lambda"; List []; Value (K 41)]]
    let astUsingLambda = List [Symbol "+"; (Value (K 1)); invokedLambda]
    match (eval env exp) with
        | Value (K k) -> Assert.AreEqual (42, k)
        | _ -> Assert.Fail()
    Assert.Pass()