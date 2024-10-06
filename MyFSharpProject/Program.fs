open System
open Parser
open Interpreter

let text = System.IO.File.ReadAllText("sample.x")
let tokens = Parser.make_token(text |> Seq.toList)
let parsed = Parser.parse(tokens |> Seq.toList)
let result = Interpreter.evaluate parsed Map.empty