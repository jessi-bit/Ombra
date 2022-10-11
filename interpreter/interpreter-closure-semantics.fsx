module Ombra.Interpreter

// ----------------------------------------------
// Ombra's interpreter - Lambda Calculus semantics

type ident = string

type exp =
    | Lit   of ident
    | Lam   of (ident * exp)
    | App   of (exp * exp)
    // --- outside Lambda Calculus
    | Bool  of bool
    | If    of (exp * exp * exp)

// alpha-conversion and beta-reduction are explained here
// https://pages.cs.wisc.edu/~horwitz/CS704-NOTES/1.LAMBDA-CALCULUS.html#beta

// TODO using De Bruijn indexes would remove the need for alpha-conversion
// and occur, but then the parsing would be a bit more difficult

// precondition: z does not occur in M
// returns M with all free occurrences of x replaced by z
let rec α M x z =
    match M with
        | Lit v when v = x      -> Lit z
        | Lit _                 -> M
        | Lam (v, _) when v = x -> M
        | Lam (v, e)            -> Lam (v, α e x z)
        | App (e, e')           -> App (α e x z, α e' x z)
        | Plus (e, e')          -> Plus (α e x z, α e' x z)
        | Cons (head, tail)     -> Cons (α head x z, α tail x z)
        | If (cond, ifBranch, elseBranch) -> If (α cond x z, α ifBranch x z, α elseBranch x z)
        | Let (v, _, _) when v = x  -> M
        | Let (v, assignment, body) -> Let (v, α assignment x z, α body x z)

// (λx.M)N → β M[N/x]
// "[...] the notation means M with all free occurrences of x replaced with N
// in a way that avoids capture. We say that (λx.M)N beta-reduces to M with N
// substituted for x."
let rec occursFree x N  =
    match N with
        | Lit y when y = x -> true
        | Lam (y, e) -> y <> x && occursFree x e
        | App (e1, e2) | Plus (e1, e2) | Cons (e1, e2) ->
            occursFree x e1 || occursFree x e2
        | If (cond, ifBranch, elseBranch) ->
            occursFree x cond || occursFree x ifBranch || occursFree x elseBranch
        | Let (y, assignment, body) -> y <> x && occursFree x assignment && occursFree x body
        | _ -> false

let identsSet e = 
    let rec idents e = 
        match e with 
            | Lit ident          -> [ident]
            | Lam (ident, exp)   -> ident :: (idents exp)
            | App (e, e')        -> (idents e) @ (idents e') 
            | Plus (e, e')       -> (idents e) @ (idents e')
            | Cons (e, e')       -> (idents e) @ (idents e')
            | If (e, e', e'')    -> (idents e) @ (idents e') @ (idents e'')
            | Let (ident, e, e') -> ident :: (idents e) @ (idents e')
            | _                  -> []
    idents e |> Set.ofList

let chooseIdent x y N e =
    let vars = Seq.initInfinite (fun num -> "X" + string num)
    let filtered usedVarsSet = Seq.filter (fun var -> not (Set.contains var usedVarsSet)) vars |> Seq.cache
    let varsSet = Set.union (identsSet N) (Set.union (identsSet e) (Set.add y (Set.add x Set.empty)))
    Seq.item 0 (filtered varsSet) 

let rec β M x N =
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
        | If (cond, ifBranch, elseBranch)  -> If ((β cond x N), (β ifBranch x N), (β elseBranch x N))
        | Let (v, _, _) when v = x         -> M
        | Let (v, assignment, body) when occursFree v N ->
            let v' = chooseIdent x v N body
            let body' = α body v v'
            Let (v', β assignment x N, β body' x N)
        | Let (v, assignment, body)                     ->
            Let (v, β assignment x N, β body x N)
        | _ -> M

let rec eval = function
    | App (lam, arg) -> let argE = eval arg
                        match (eval lam) with
                            | Lam (var, body) -> eval (β body var argE)
                            |  _ -> failwith "Error lambda"  
    | e -> e                     

// ----------------------------------------------
// Ombra's interpreter - Lisp

// TODO add Bool and Str
type value =
    | Clos of (ident * exp * env)
    | Num  of float
    | Boo  of bool
    | Lst  of value list
and env = Map<ident,value>

// TODO import our way to deal with functions?

// evalOmbra
let rec evalO env = function
    | Lit v -> Map.find v env
    | Lam (ident, body) -> Clos (ident, body, env)
    | App e -> let βreduced = eval (App e)
               evalO env βreduced
    // the idea is to use the model of computation embodied by the
    // lambda calculus to perform the actual computations
    | Const f -> Num f
    | Bool b -> Boo b
    | Plus (e, e') -> match (evalO env e, evalO env e') with
                          | (Num n, Num n') -> Num (n + n')
    | Cons (head, tail) -> let head' = evalO env head
                           let (Lst tail') = match tail with
                                                 | Nil -> Lst []
                                                 | _   -> evalO env tail
                           Lst (head' :: tail')
    | If (cond, ifBranch, elseBranch) ->
        let (Boo cond') = evalO env cond
        if cond' then evalO env ifBranch else evalO env elseBranch
    | Let (ident, assignment, body) ->
        // TODO is this lazy?
        let assignment' = evalO env assignment
        let env' = Map.add ident assignment' env
        evalO env' body