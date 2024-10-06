module Parser

open System
open System.Numerics
open System.IO

type env = Map<string, expr>

and expr = 
    | None
    | Application of expr * expr
    | Lambda of string * expr
    | Var of string
    | Int of int
    | Bool of Boolean
    | String of string
    | PredefinedFunction of string
    | Conditional of expr * expr * expr
    | LocalVar of string * expr * expr
    | LocalVarRecursive of string * expr * expr
    | Partial of string * int * expr list
    | Closure of expr * env
    | RecursiveClosure of expr * env * string

let behaviour = function
    | "&" -> (function [Bool(a);Bool(b)] -> Bool(a&&b))
    | "|" -> (function [Bool(a);Bool(b)] -> Bool(a||b))
    | "!" -> (function [Bool(a)] -> Bool(not a))

    | "+" -> (function [Int(a);Int(b)] -> Int(a+b))
    | "-" -> (function [Int(a);Int(b)] -> Int(a-b))
    | "*" -> (function [Int(a);Int(b)] -> Int(a*b))
    | ":" -> (function [Int(a);Int(b)] -> Int(a/b))

    | "=" -> (function [Int(a);Int(b)] -> if a=b then Bool(true) else Bool(false))
    | "!=" -> (function [Int(a);Int(b)] -> if a<>b then Bool(true) else Bool(false))
    | ">" -> (function [Int(a);Int(b)] -> if a>b then Bool(true) else Bool(false))
    | ">=" -> (function [Int(a);Int(b)] -> if a>=b then Bool(true) else Bool(false))
    | "<" -> (function [Int(a);Int(b)] -> if a<b then Bool(true) else Bool(false))
    | "<=" -> (function [Int(a);Int(b)] -> if a<=b then Bool(true) else Bool(false))

    | "print" -> (function [expr.String(a)] -> printf "%s" a; None)
    | "printint" -> (function [expr.Int(a)] -> printf "%d" a; None)
    | _ -> (function [] -> None)

let funArgs = function
    | "&" -> 2
    | "|" -> 2
    | "!" -> 1

    | "+" -> 2
    | "-" -> 2
    | "*" -> 2
    | ":" -> 2

    | "=" -> 2
    | "!=" -> 2
    | ">" -> 2
    | "<" -> 2
    | ">=" -> 2
    | "<=" -> 2

    | "print" -> 1 
    | "printint" -> 1
    | "fun" -> 1


let PredefinedFuncs = 
    [
    "&"
    "|"
    "!"
    "+";
    "-";
    "*";
    ":";
    "=";
    ">";
    ">=";
    "<";
    "<=";
    "!="
    "print";
    "printint";
    "fun";
    ]



type Token = 
    | Number of string
    | String of string
    | Action of string
    | OpenBlock 
    | CloseBlock
    | OpenBracket 
    | CloseBracket
    | Comma


let make_token input = 

    let is_token_end = function
        | ')' | '(' -> true
        | x when Char.IsWhiteSpace(x) -> true
        | _ -> false

    let rec parse_action buffer = function
        | [] -> buffer, []
        | x::tail when is_token_end x -> buffer, (x::tail)
        | x::tail -> parse_action (buffer + x.ToString()) tail

    let rec parse_number buffer = function
        | [] -> buffer, []
        | x::tail when Char.IsDigit(x) -> parse_action (buffer + x.ToString()) tail
        | x::tail -> buffer, (x::tail)

    let rec parse_string buffer = function
        | [] -> buffer, []
        | '\"'::tail -> buffer, tail
        | x::tail when Char.IsWhiteSpace(x) -> parse_string buffer tail
        | x::tail -> parse_string (buffer + x.ToString()) tail





    let rec make_token buffer = function
        | [] -> Seq.rev buffer
        | '('::tail -> make_token (Token.OpenBracket::buffer) tail
        | ')'::tail -> make_token (Token.CloseBracket::buffer) tail
        | '\\'::tail -> make_token (Token.OpenBlock::buffer) tail
        | '/'::tail -> make_token (Token.CloseBlock::buffer) tail
        | ','::tail -> make_token (Token.Comma::buffer) tail
        | x::tail when Char.IsWhiteSpace(x) -> make_token buffer tail
        | '\"'::tail ->
            let value, rest = parse_string "" tail
            make_token (Token.String(value)::buffer) rest
        | x::tail when Char.IsDigit(x) ->
            let value, rest = parse_number (x.ToString()) tail
            make_token (Token.Number(value)::buffer) rest
        | x::tail ->
            let value, rest = parse_action (x.ToString()) tail
            make_token (Token.Action(value)::buffer) rest
    make_token [] input


let parse input_tokens = 
    let rec process_block = function
        | Token.CloseBlock::tail -> None, tail
        | Token.CloseBracket::tail -> None, tail

        | Token.Action(act)::Token.OpenBracket::rest when act = "if" -> 
            let condition, next = process_block rest
            let true_branch, remaining_after_true = process_block(List.tail next)
            let falseBranch, remaining_after_false = process_block(List.tail (List.tail remaining_after_true))
            expr.Conditional(condition, true_branch, falseBranch), remaining_after_false

        | Token.Action(op)::Token.Action(nm)::Token.OpenBracket::Token.Action(arg)::Token.CloseBracket::Token.OpenBlock::rest when op = "fun" ->
            let body, next_tokens = process_block rest
            let continuation, remaining_tokens = process_block (List.tail next_tokens)
            expr.LocalVarRecursive(nm, expr.Lambda(arg, body), continuation), remaining_tokens

        | Token.Action(act)::Token.OpenBracket::rest ->
            let argument, following = process_block rest
            let application, other_tokens = process_block following
            let variable = Var(act)
            match application with
                | None -> expr.Application(variable, argument), other_tokens
                | _ -> expr.Application(variable, argument), other_tokens

        | Token.Action(op)::tail when List.contains op PredefinedFuncs ->
            let app, tokens_after_app = process_block tail
            expr.Application(expr.PredefinedFunction(op), app), tokens_after_app

        | Token.Action(varName)::remaining_tokens ->
            let value, next_tokens = process_block remaining_tokens
            let var_eq = expr.Var(varName)
            match value with
                | None -> var_eq, next_tokens
                | _ -> expr.Application(value, var_eq), next_tokens

        | Token.Number(numValue)::remaining_tokens ->
            let func, rest = process_block remaining_tokens
            let num_eq = expr.Int(numValue |> int)
            match func with
                | None -> num_eq, rest
                | _ -> expr.Application(func, num_eq), rest

        | Token.String(strValue)::remaining_tokens ->
            let func, rest = process_block remaining_tokens
            let str_eq = expr.String(strValue)
            match func with
                | None -> str_eq, rest
                | _ -> expr.Application(func, str_eq), rest

        | unexpected::rest -> 
            printfn "Unexpected token: %A" unexpected
            None, rest

        | [] -> 
            None, []
            
    let result, trash = process_block input_tokens
    result