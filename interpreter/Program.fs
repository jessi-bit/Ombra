let tokenize (code: string) =
    code
        .Replace("(", " ( ")
        .Replace(")", " ) ")
        .Trim()
        .Split(" ")

let parseFloat s =
    try
        let f = System.Single.Parse s
        Some f
    with _ -> None

type OToken =
    | OLiteralFloat of f : float32
    | OLiteralString of s : string
    | OIdentifier of s : string

type Expression =
    | OToken
    | List of Expression


let categorize token =
    match parseFloat token with
        | Some f -> OLiteralFloat f
        | None -> match token with
                      | s when s.StartsWith "\"" && s.EndsWith "\"" -> OLiteralString (s.Substring(1, (s.Length) - 1))
                      | s -> OIdentifier s


// let parenthesize tokens result =
//     match List.head tokens with
//     | OIdentifier "(" -> let innerResult = parenthesize List.tail tokens 
//                          parenthesize list.Tail tokens result @ innerResult
//     | OIdentifier ")" -> result
//     | _ ->

let parenthesize tokens result =
    match List.head tokens with
    | OIdentifier "(" -> let innerResult = parenthesize List.tail tokens 
                         parenthesize list.Tail tokens result @ innerResult
    | OIdentifier ")" -> result
    | _ ->






// TODO add custom types to tokenised and parenthesized?
(*

let rec parenthesize tokenised parenthesized =
    match tokenised with =
        |
*)

let result = tokenize "(+ 1 (+ 1 40))"
(*
[
    OIdentifier "+"; OLiteralFloat 1;
    [
        OIdentifier "+"; OLiteralFloat 1; OLiteraFloat 40
    ]

]
*)

