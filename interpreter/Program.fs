let tokenize (code: string) =
    let result = code.Replace("(", " ( ").Replace(")", " ) ").Trim().Split(" ")
    Array.toList result

let parseFloat s =
    try
        let f = System.Single.Parse s
        Some f
    with _ -> None

type OToken =
    | OLiteralFloat of float32
    | OLiteralString of string
    | OIdentifier of string

type Expression<'s> =
    | Nil 
    | List of List<'s> * Expression<'s> 

let exp1 = List (["2", "+", "1"], List (["3", "+", "5"], Nil))
printfn "Expression %O" exp1


let categorize token =
    match parseFloat token with
        | Some f -> OLiteralFloat f
        | None -> match token with
                      | s when s.StartsWith "\"" && s.EndsWith "\"" -> OLiteralString (s.Substring(1, (s.Length) - 1))
                      | s -> OIdentifier s

// let rec parenthesizeDiff tokens result =
//     match tokens with
//     | [] -> Nil
//     | head::tail when head = "(" -> 
    
let rec parenthesize tokens result =
    match tokens with
        | [] -> result
        | head::tail when head = "(" -> parenthesize tail result
        | head::_ when head = ")" -> result
        | head::tail -> parenthesize tail (result @ [(categorize head)])

let result = parenthesize (tokenize "(+ 1 41)") []
