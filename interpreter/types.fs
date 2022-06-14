module Ombra.Types

type var = string
            
type vname = string

type atom =
    | K of int
    | B of bool
    | S of string
    | Var of vname
    | None
    | Nil

type exp = element list
and element =
    | Atom of atom
    | List of exp
    | Op of string
    | SubExp of exp
    | L of lambda
    | ITE of element * element * element
and lambda =
    | LambdaDef of vname list * element
    | LambdaApp of lambda * element

type env = E of Map<vname, element>

// ---------------------------------------------
// AST stringifying
//

let rec lispyList xs =
    match xs with
        | [] -> ""
        | [el] -> string el 
        | head :: tail ->
            string head + " " + lispyList tail

let rec toStringEl element =
        match element with 
            | Atom a -> match a with
                            | K i -> sprintf "%i " i
                            | B(b) -> 
                                match b with
                                    | true -> sprintf "%s" "#t "
                                    | _ -> sprintf "%s" "#f "

                            | S(s) -> s + " "
                            | Var(v) -> v + " "
                            | None -> sprintf "%s" "none"
                            | Nil -> ""
            | List [] -> sprintf "%s" "()"
            | List (head :: tail) -> 
                let res =  "(" + (toStringEl head) + (toStringExp tail)
                let res' = res.Replace(" )", "")
                sprintf "%s" (res'.TrimEnd() + ")")
            | Op o -> 
                match o with
                    | "'" -> o
                    | _ -> o + " "
            | SubExp e -> sprintf "%s " (toStringExp e)
            | L lambda -> 
                match lambda with
                    | LambdaDef (args, body) -> 
                        sprintf "((%s (%s) %s)" "lambda" (lispyList args) ((toStringEl body).TrimEnd())
                    | LambdaApp (LambdaDef (_,_) as def, parms) ->
                        let stringyParms = match parms with
                                            | List _ -> (toStringEl parms).Replace(")", "").Replace("(","")
                                            | _ -> toStringEl parms
                        sprintf "%s %s" (toStringEl (L def)) stringyParms    
                    | _ -> ""
and toStringExp exp =
    match exp with
        | [] -> ""
        | [el] -> 
            let s = (toStringEl el).Trim()
            sprintf "%s)" s
        | head :: tail -> 
            let res = (toStringEl head) + (toStringExp tail)
            match head with
                | Op _ -> sprintf "(%s" res
                | _ -> sprintf "%s" res
