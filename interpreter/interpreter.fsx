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
    | Nil
    | Cons  of (exp * exp)


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
        | Plus (e, e')          -> Plus (α e x z, α e' x z)
        | Cons (head, tail)     -> Cons (α head x z, α tail x z)

// (λx.M)N → β M[N/x]
// "[...] the notation means M with all free occurrences of x replaced with N
// in a way that avoids capture. We say that (λx.M)N beta-reduces to M with N
// substituted for x."
let rec occursFree (x : string) (N : exp)  =
    match N with
        | Lit y when y = x -> true
        | Lam(y, e) -> y <> x && occursFree x e 
        | App (e1, e2) | Plus (e1, e2) | Cons (e1, e2) ->
            occursFree x e1 || occursFree x e2
        | _ -> false
  
occursFree "x" (Lam ("x", Lit "x")) // false
occursFree "y" (Lam ("x", Lit "x")) // false because y is not in the lambda body
occursFree "y" (Lam ("x", Lit "y")) // true
occursFree "z" (Lam ("w", (Lam ("x", (Lam ("y", Lit "y")))))) //false
occursFree "z" (Lam ("w", (Lam ("x", (Lam ("y", Lit "x")))))) //false
occursFree "y" (Lam ("x", Plus (Lit "x", Lit "y"))) //true
occursFree "x" (Lam ("x", Plus (Lit "x", Const 2))) //false

//TODO
let rec chooseIdent x y N e =
    "?"

let rec β (M : exp) (x : string) (N : exp) =
    match M with
        | Lit v when v = x                 -> N
        | Lit _                            -> M
        | Lam (v, _) when v = x            -> M
        | Lam (v, e) when occursFree v N   ->
            let v' = chooseIdent x v N e
            let e' = α e v v'                    
            Lam (v', β e' x N)
        | Lam (v, e)                       -> Lam (v, β e x N)    
        | App (e, e')                      -> App ((β e x N), (β e' x N))  
        // --- outside Lambda Calculus
        | Plus (e, e')                     -> Plus ((β e x N), (β e' x N))
        | Cons (e, e')                     -> Cons ((β e x N), (β e' x N))
        | _ -> M

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

// TODO add Bool and Str
type value =
    | Clos of (ident * exp * env)
    | Num  of float
    | Lst of value list
and env = Map<ident,value>

// TODO import our way to deal with functions?

// evalOmbra
let rec evalO env = function
    | Lit v -> Map.find v env
    | Const f -> Num f
    | Lam (ident, body) -> Clos (ident, body, env)
    // the idea is to use the model of computation embodied by the
    // lambda calculus to perform the actual computations
    | App e -> let reduced = eval (App e)
               evalO env reduced
    | Plus (e, e') -> match (evalO env e, evalO env e') with
                          | (Num n, Num n') -> Num (n + n')
    | Cons (head, tail) -> let head' = evalO env head
                           let (Lst tail') = match tail with
                                                 | Nil -> Lst []
                                                 | _   -> evalO env tail
                           Lst (head' :: tail')

let cons = App (Lam ("x", Cons (Lit "x", Nil)), Const 42)
evalO Map.empty cons

let sum = App (Lam ("x", App (Lam ("y", Plus (Lit "x", Lit "y")), Const 41)), Const 1)
evalO Map.empty sum

let sumFree = Lam ("x", App (Lam ("y", Plus (Lit "x", Lit "y")), Const 41))
evalO Map.empty sumFree

evalO (Map.add "x" (Num 1) Map.empty) (Plus (Lit "x", Const 41))
