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

let categorize token =
    match parseFloat token with
        | Some f -> OLiteralFloat f
        | None -> match token with
                      | s when s.StartsWith "\"" && s.EndsWith "\"" -> OLiteralString (s.Substring(1, (s.Length) - 1))
                      | s -> OIdentifier s

let rec parenthesize tokens result =
    match tokens with
        | [] -> result
        | head::tail when head = "(" -> parenthesize tail result
        | head::_ when head = ")" -> result
        | head::tail -> parenthesize tail (result @ [(categorize head)])

let result = parenthesize (tokenize "(+ 1 41)") []
