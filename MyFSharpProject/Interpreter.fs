module Interpreter

open Parser

let rec evaluate exp env = 
    match exp with
    | None -> None
    | Parser.expr.Application(ex1, ex2) -> apply (evaluate ex1 env) (evaluate ex2 env)
    | Parser.expr.Lambda(string, ex) -> Parser.expr.Closure(ex, env)
    | Parser.expr.Var(x) -> Map.find x env
    | Parser.expr.Int(x) -> Parser.expr.Int(x)
    | Parser.expr.String(x) -> Parser.expr.String(x)
    | Parser.expr.PredefinedFunction(string) -> Parser.expr.Partial(string, (funArgs string), [])
    | Parser.expr.Conditional(e0, e1, e2) -> 
        match evaluate e0 env with
        | Parser.expr.Int(0) -> evaluate e2 env
        | Parser.expr.String("") | None -> evaluate e2 env
        | Parser.expr.Bool(false) -> evaluate e2 env
        | _ -> evaluate e1 env
    | Parser.expr.LocalVar(string, e1, e2) -> 
        let e1' = evaluate e1 env in evaluate e2 (Map.add string e1' env)
    | Parser.expr.LocalVarRecursive(string, e1, e2) -> 
        let e1' = Parser.expr.RecursiveClosure(e1, env, string) in  evaluate e2 (Map.add string e1' env)

and apply e1 e2 = 
    match e1 with
    | Parser.expr.Closure(Parser.expr.Lambda(v, e), env) -> evaluate e (Map.add v e2 env)
    | Parser.expr.RecursiveClosure(Parser.expr.Lambda(v, e), env, string) -> evaluate e (Map.add v e2 (Map.add string e1 env))
    | Parser.expr.Partial(string, x, args) ->
        if x=1 then (behaviour string) (e2::args)
        else Parser.expr.Partial(string, x-1, e2::args)