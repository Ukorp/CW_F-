# Функциональное программирование: Пишем компилятор!

Цель этой лабораторной работы - придумать свой собственный функциональный язык программирования и разработать для него интерпретатор или компилятор.

В данной работе участвовали 2 человека - компилятор/интерпретатор + примеры программ + краткая документация в README.md

## Синтаксис

В данной работе были реализованы следующие бинарные операторы:

`& | !` \\ логические операторы

`+ - * :` \\ алгебраические операторы

`= != < > <= >=` \\ операторы сравнения

А так же функции

`println` \\ вывод строки в консоль
`printint` \\ вывод числа в консоль

Операторы `if` `else`, а так же функции `fun` используют `\` в качестве открывающей и `/` в качестве закрувыющей скобки.
Условие должно быть записано в скобках

Пример:

```
if (*condition*) \
  *case true*
/ else \
  *case false*
\
```

```
fun function(*args*) \
  *...code...*
/
```

## Пример кода

При необходимости добавления `else if` как в C-подобных языках - используем else \ if \ *code* / /

### Факториал
```
fun fact (x) \
  if (x = 1) \
        1
  / else \
  	if (x = 0) \
  		1
  	/ else \
          	x * fact(x - 1)
      /
/

printint fact(8)
```
## Какие особенности языка мы реализовали:

-   [ ] Именованные переменные (`let`)
-   [x] Рекурсия
-   [ ] Ленивое вычисление
-   [x] Функции
-   [x] Замыкания
-   [ ] Библиотечные функции: ввод-вывод файлов
-   [ ] Списки / Последовательности
-   [ ] Библиотечные функции: списки/последовательности

## Структура проекта

Проект состоит из трёх файлов для реализации проекта и одного файла с кодом для интерпритации:

1.  `Program.fs`: основной файл, с которого происходит запуск программы
2.  `Interpreter.fs`: реализация интерпретатора
3.  `Parser.fs`: реализация парсера
4.  `sample.x`: файл с кодом который нужно интерпритировать 

### `Parser.fs`

Данный код реализует модуль парсера для простого языка выражений на F#

Описание:
- Создаются типы данных для представления различных выражений (числа, строки, логические значения, функции и т.д.).
- Функция make_token разбивает входную строку на токены (числа, строки, операторы, скобки и т.д.), которые будут использоваться в процессе парсинга.
- Функция parse обрабатывает список токенов и строит абстрактное синтаксическое дерево (AST), представляющее выражения на основе их структуры (например, условные конструкции, функции).
- Определяются предопределенные функции и операторы (логические, арифметические), которые могут быть использованы в выражениях.
- Включена базовая обработка ошибок для неожиданных токенов.

Код:
```
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
```

### `Interpreter.fs`

Данный код реализует интерпретатор для языка выражений, определенного в модуле Parser.
Описание:
1. Оценка выражений:
  - Функция evaluate рекурсивно обрабатывает различные типы выражений:
    - Применение функций: Вычисляет результат применения функции к аргументу.
    - Лямбда-функции: Возвращает замыкание с текущим окружением.
    - Переменные: Ищет значение переменной в окружении.
    - Числа и строки: Возвращает их как есть.
    - Предопределенные функции: Создает частичное применение функции.
    - Условные конструкции: Оценивает условие и выбирает соответствующий блок.
2. Применение функций:
  - Функция apply обрабатывает применение функций к аргументам:
  - Вызывает лямбда-функцию с добавлением аргумента в окружение.
  - Обрабатывает рекурсивные замыкания.
  - Управляет частичным применением функций.

Код:
```
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
```

### `Program.fs`

Основная запускаеммая программа

Описание:
- Чтение файла: Считывает содержимое файла sample.x в виде строки.
- Токенизация: Преобразует строку в список токенов с помощью функции Parser.make_token.
- Парсинг: Преобразует список токенов в абстрактное синтаксическое дерево (AST) с помощью функции Parser.parse.
- Оценка выражения: Вычисляет результат, используя интерпретатор Interpreter.evaluate, начиная с пустого окружения (Map.empty).

Код:
```
open System
open Parser
open Interpreter

let text = System.IO.File.ReadAllText("sample.x")
let tokens = Parser.make_token(text |> Seq.toList)
let parsed = Parser.parse(tokens |> Seq.toList)
let result = Interpreter.evaluate parsed Map.empty
```

## Реализация особенностей языка

### Функции

Функции записываются при помощи ключевого слова `fun` и используют `\` в качестве открывающей и `/` в качестве закрувыющей скобки. Функция имеет своё имя, принимает аргументы и возвращает результат. 

Код:
```
fun function(*args*) \
  *...code...*
/
```

### Рекурсия

Рекурсия определена при помощи функций, вызывающих самих себя. Реализация рекурсии представлена в файле sample.x.

### Замыкания

Замыкания реализованы через лямбда функции, способные захватить элементы вне функции.

## Генеративный ИИ

Генеративный ИИ ChatGPT использовался того чтобы более подробно описать принцип работы некоторых функций в данном проекте специально для README.md файла. Так же ИИ подсказывал, в каких местах находятся ошибки в коде и как их лучше исправлять.

## Авторы

| Имя                             | Роль в проекте                                                                           |
| ------------------------------- | ---------------------------------------------------------------------------------------- |
| Сафин Тимур Айратович           | Разработка синтаксиса языка, парсера, а так же оформление работы                         |
| Шишкин Артём Константинович     | Разработка интерпритатора и основной программы, помощь в разработке парсера и синтаксиса |
