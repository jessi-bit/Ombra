module Ombra.ParserTest

open NUnit.Framework
open Ombra.Types
open Ombra.Ops
open Ombra.Interpreter
open Ombra.Parser

//open FsCheck

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestConfig () =
    Assert.Pass()

[<Test>]
let TestEmptyExp () =
    // ()
    let exp = []
    let env = E Map.empty
    match (parse exp) with
        
        | _ -> Assert.Fail()
    Assert.Pass()