// ----------------------------------------------
// Ombra's interpreter - Lambda Calculus kernel

// notes for us, we will remove this before publishing:
// * our implementation is lazy
// * our interpreter is not defensive

type ident = string
type index = int

type exp =
    | Lit   of ident
    | Lam   of (ident * exp)
    | App   of (exp * exp)
    // --- outside Lambda Calculus
    | Const of float
    | Plus  of (exp * exp)


// alpha-conversion and beta-reduction are explained here
// https://pages.cs.wisc.edu/~horwitz/CS704-NOTES/1.LAMBDA-CALCULUS.html#beta
// TODO using De Bruijn indexes would remove the need for alpha-conversion
// and occur, but then the parsing would be a bit more difficult

// precondition: z does not occur in M
// returns M with all free occurrences of x replaced by z
let rec α (M : exp) (x : string) (z : string) =
    match M with
        | Lit v when v = x      -> Lit z
        | Lit _                 -> M
        | Lam (v, _) when v = x -> M
        | Lam (v, e)            -> Lam (v, α e x z)
        | App (e, e')           -> App (α e x z, α e' x z)

// (λx.M)N → β M[N/x]
// "[...] the notation means M with all free occurrences of x replaced with N
// in a way that avoids capture. We say that (λx.M)N beta-reduces to M with N
// substituted for x."

// * TODO check for correctness
//  my definition of a free variable is:
//  a variable that does not occur anywhere in the nested structure of Lam
//  as a parameter
//
//  λx.λy.x - has a free variable, y
//  λx.λy.xy - does not have free variables
//  
// * TORESPOND 
// The definition and examples are correct in my opinion. I would just add :
// In the expression λx.M, every x in M is bound; every variable other than x that is free in M 
// is free in λx.M.
// In the expression MN the free variables of MN are the union of two sets: 
// the free variables of M, and the free variables of N. (compitino in April).
// Given (λx.y)(λy.yx) the free variables are {y, x}.

let rec occursFree (x : string) (N : exp)  =
  match N with
    | Lit y when y = x -> true 
    | Lam(y, e) -> y <> x && occursFree x e 
    | App(e1, e2) | Plus (e1, e2) -> occursFree x e1 || occursFree x e2 
    | _ -> false

occursFree "x" (Lam ("x", Lit "x")) // false
occursFree "y" (Lam ("x", Lit "x")) // false because y is not in the lambda body
occursFree "y" (Lam ("x", Lit "y")) // true
occursFree "z" (Lam ("w", (Lam ("x", (Lam ("y", Lit "y")))))) //false
occursFree "z" (Lam ("w", (Lam ("x", (Lam ("y", Lit "x")))))) //false
occursFree "y" (Lam ("x", Plus (Lit "x", Lit "y"))) //true

// TODO
let rec chooseIdent x y N e =
    "?"

let rec β (M : exp) (x : string) (N : exp) =
    printf "M: %A\n x: %A\n N: %A\n" M x N
    match M with
        | Lit v when v = x                 -> N
        | Lit _                            -> M
        | Lam (v, _) when v = x            -> M  //No free occurrences of x in M
        | Lam (v, e) when occursFree v N ->
            let v' = chooseIdent x v N e
            let e' = α e v v'                    
            Lam (v', β e' x N)
        | Lam (v, e)                       -> Lam (v, β e x N)    
        | App (e, e')                      -> App ((β e x N), (β e' x N))  
        // --- outside Lambda Calculus
        | Plus (e, e')                     -> Plus((β e x N), (β e' x N))
                        //   printf "PROBLEM %A - %A - %A\n" x red red'
                        //   match (red, red') with
                        //       | (Lit v, Const c) when v = x -> Const c
                        //       | (Lit _, Const _)            -> M
                        //       | _ -> M
        | _ -> M

// let rec areThereFreeVars = function
//     | Lit _ -> true
//     | Const _ -> false
//     | Lam (var, body) -> 
//     | Plus (e1, e2) | App (e1, e2) -> 
//         areThereFreeVars e1 || areThereFreeVars e2

let rec eval = function
    | App (lam, arg) -> let argE = eval arg
                        match (eval lam) with
                            | Lam (var, body) -> eval (β body var argE)
                            |  _ -> failwith "Error lambda"  
    | e -> e                     


//**(λx.x)y
let simple = Lam ("x", Lit "x")
eval (App (simple, Const 2))

//--------------------------------------------------------------------------------------------------------
//**((λx.λy.x)y)z
let another = App (App (Lam ("x", Lam ("y", Lit "x")), Lit "y"), Lit "z")
eval another 
//Steps in reduction
β (Lam ("y", Lit "x")) "x" (Lit "y") //Lam ? -> y
β (Lam ("?", Lit "y")) "?" (Lit "z")  //Lam ? -> y
β (Lit "y") "?" (Lit "z") 

//---------------------------------------------------------------------------------------------------------
//**(λx.x + 2)3
let plus0 = App (Lam ("x", Plus (Lit "x", Const 2)), Const 3)
eval plus0  
//Steps in reduction
β (Plus (Lit "x", Lit "y")) "x" (Const 3) // (3 + y)

//----------------------------------------------------------------------------------------------------------
//**(λx.x+y)3
let plus01 = App (Lam ("x", Plus (Lit "x", Lit "y")), Const 3)
eval plus01

//----------------------------------------------------------------------------------------------------------
//**((λx.λy.x + y)2)3)
let innerBody = Lam ("y", Plus(Lit "x", Lit "y"))
let plus1 = App (App (Lam ("x", innerBody), Const 2), Const 3)
eval plus1

// ----------------------------------------------
// Ombra's interpreter - Lisp

// TODO I did not check the following, I focused on the lambda calculus part

// TODO add Bool and Str
type value =
    | Clos of (ident * exp * env)
    | Num  of float
and env = Map<ident,value>

// TODO import our way to deal with functions?
// I dont think we should be dealing with Lits in evalO

// evalOmbra
// | Plus (e1, e2) -> 
    //     match (eval e1, eval e2) with
    //         | (Const c, Lit x) -> Plus (Const c, Lit x)
    //         | (Lit x, Const c) -> Plus (Lit x, Const c)
    //         | (Lit x, Lit y) -> Plus (Lit x, Lit y)
    //         | (Const c1, Const c2) -> Const (c1 + c2)
    //         | _ -> failwith "Not implemented"
    
let rec evalO env = function
    | Const f -> Num f
    | Lam (ident, body) -> Clos (ident, body, env)
    // the idea is to use the model of computation embodied by the
    // lambda calculus to perform the actual computations
    | App e -> let reduced = eval (App e)
               printf "REDUCED %A\n" reduced
               evalO env reduced
    | Plus (e, e') -> match (evalO env e, evalO env e') with
                          | (Num n, Num n') -> Num (n + n')
    | e -> failwith (sprintf "MISSING %A\n" e)

let sum = App (Lam ("x", App (Lam ("y", Plus (Lit "x", Lit "y")), Const 41)), Const 1)
evalO Map.empty sum


// ---------------------------------------------------------
// WHAT FOLLOWS ARE PROF MOMIGLIANO's approaches

// type ident = string

// type expr =
//     | Lam   of (ident * expr)
//     | App   of expr * expr
//     | Const of int
//     | Lit   of ident
//     | Let   of (ident * expr * expr)

// // why this?
// type value = Clos of (ident * expr * env) | Num of int
// and env = Map<ident,value>

// let rec eval c e =
//     match e with
//         | Const n -> Num n // why this?
//         | Lit x -> Map.find x c
//         | Lam (x, e) -> Clos(x, e, c)
//         | Let (x, e1, e2) -> // single variable let
//             let v = eval c e1
//             let c' = Map.add x v c
//             eval c' e2
//         | App (e1, e2) ->
//             let v2 = eval c e2
//             match eval c e1 with
//             | Clos(x, e, cf) ->
//                 let cc = Map.add x v2 cf
//                 eval cc e
//             | _ -> failwith "type error"



// let ex = eval Map.empty (App (Lam ("x", Lit "x"), Const 42))
// let ex1 = eval Map.empty (App (Lam ("x", Lam ("y", Lit "x")), Const 42))



// // -----------------------

// type sexp = Sat of string | Fat of float | Dot of (sexp * sexp)
// let (@@) e1 e2 = Dot(e1, e2)
// type Env = Map<string,sexp>

// let nil = Sat "nil"
// let isTrue x = x <> nil
// let Define = Sat "define"
// let If = Sat "if"

// // to be completed
// let unaryEnv =
//     Map.add "sqrt" sqrt Map.empty

// let binaryEnv =
//     Map.add "*" (fun x y -> x * y : float)
//     (Map.add "+" (fun x y -> x + y : float) Map.empty)


// let rec eval e (env : Env) =
//     match e with
//         | Fat x -> Fat x, env
//         | Sat x -> Map.find x env, env
//         | Dot(s1,s2) ->
//             match s1 with
//                 | Sat "if" ->
//                     match s2 with
//                         | Dot(test,Dot(e1,e2)) ->
//                             let (v,envv) = eval test env
//                             if isTrue v then eval e1 envv else eval e2 envv
//                         | _ -> failwith "error"
//                 | Sat "define" ->
//                     match s2 with
//                         | Dot(Sat x,e) ->
//                             let (v,envv) = eval e env
//                             v,Map.add x v env
//                         | _ -> failwith "error"
//                 | Sat "uf" ->
//                     match Map.tryFind "uf" unaryEnv with
//                         | Some op -> match (eval s2 env) with
//                             | Fat n, envf -> Fat (op n),envf
//                             | _ -> failwith "type error"
//                         | None -> failwith "type error"
//         // add binary ops
//         //
//         | _ -> failwith "not implemented yet"

// let p1 = Define @@ (Sat "x") @@ (Fat 4.)
// let t1 = eval p1 Map.empty
// let p2 = (Sat "sqrt") @@ (Fat 4.)
// // noy quite right
// let t2 = eval p2 Map.empty
