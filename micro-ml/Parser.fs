module Parser

open FSharp.Core
open Absyn
open FParsec

let t3tuple ((x,y),z) = (x, y, z)
let t4tuple (((x,y),z),f) = (x, y, z, f)

let ex, exRef = createParserForwardedToRef<expr, unit>()
let topEx, topExRef = createParserForwardedToRef<expr, unit>()

let s = opt spaces
let ws x = s >>. x .>> s

let lpar = pchar '(' |> ws
let rpar = pchar ')' |> ws
let lsb = pchar '[' |> ws
let rsb = pchar ']' |> ws
let op = ["+"; "-"; "*"; "/"; "%"; "<"; "="; ">"; ">="; "<="] |> List.map pstring |> choice |> ws
let eq = pchar '=' |> ws

let iff = pstring "if" |> ws
let thn = pstring "then" |> ws
let els = pstring "else" |> ws
let lett = pstring "let" |> ws
let inn = pstring "in" |> ws
let endd = pstring "end" |> ws

let keywords = [iff; thn; els; lett; inn; endd]

let name = 
    notFollowedBy iff >>.
    notFollowedBy thn >>.
    notFollowedBy els >>.
    notFollowedBy lett >>.
    notFollowedBy inn >>.
    notFollowedBy endd >>.
    regex "[a-zA-Z][a-zA-Z0-9]*"
    |> ws

let cstint = pint32 |> ws |>> CstI

let primFollowing = many1 (op .>>. ex) |>> List.rev |>> List.foldBack (fun (o, e) a -> Prim(o, a, e))
let prim = ex .>>. primFollowing |>> (fun (x, f) -> f x)

let ifExpr = iff >>. topEx .>> thn .>>. topEx .>> els .>>. topEx |>> t3tuple |>> If 

let varUse = name |>> Var
let varLet = lett >>. name .>> eq .>>. topEx .>> inn .>>. topEx .>> endd |>> t3tuple |>> Let
let funLet = lett >>. name .>>. name .>> eq .>>. topEx .>> inn .>>. topEx .>> endd |>> t4tuple |>> Letfun

let appFollowing = many1 ex |>> List.rev |>> List.foldBack (fun v a -> Call(a, v))
let appExpr = ex .>>. appFollowing |>> (fun (x, f) -> f x)

do exRef := ws (choice [
    attempt (lpar >>. prim .>> rpar)
    attempt (lpar >>. appExpr .>> rpar)
    attempt funLet
    varLet
    ifExpr
    varUse
    cstint
])

do topExRef := ws (attempt prim <|> attempt appExpr <|> attempt ex)

let parser = topEx .>> FParsec.CharParsers.eof

exception ParserException of string

let fromString s =
    match run parser s with
    | Success (result, _, _) -> Result.Ok result
    | Failure (err, _, _) -> Result.Error (sprintf "Failed to parse: %A" err)
