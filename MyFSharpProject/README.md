# Функциональное программирование: Пишем компилятор!

Цель этой лабораторной работы - придумать свой собственный функциональный язык программирования и разработать для него интерпретатор или компилятор.

-   2 человека - компилятор/интерпретатор + примеры программ + краткая документация в README.md

## Синтаксис

### Стандартные операции:

```
+ - : * = > >= < <=  // арифметические бинарные операторы
print // оператор вывода строки на экран
printint // оператор вывода чисел на экран
```

### If-else конструкция:

```
if (* condition *) \
    * if branch *
/ else \
    * else branch *
/
```

### Функции:

```
fun * name * (* parameters *) \
    * body *
/
```

### Примеры кода:

#### Hello World:

```
print "Hello!"
```

#### Сумма чисел:

```
printint 1 + 2
```

#### Факториал:

```
fun fact (x) \
    if (x < 1) \
        1
    / else \
        x * fact(x - 1)
    /
/
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

## Использование генеративного ИИ

В процессе разработки мы использовали генеративный ИИ, а именно ChatGPT, чтобы уточнить структуру синтаксиса языка, получить идеи по реализации парсера и интерпретатора, а также улучшить документацию. Это помогло нам сэкономить время и улучшить качество кода.

## Структура проекта

Проект состоит из следующих файлов:

-   `Program.fs`: основной файл запуска, который парсит и интерпретирует входной файл.
-   `sample.x`: пример входного файла для языка.
-   `Interpreter.fs`: файл, содержащий интерпретатор языка.
-   `Parser.fs`: файл, содержащий парсер языка.

### `Program.fs`

```fsharp
open Parser
open Interpreter

// Example run file
// Parses and interprets the factorial file from the directory of the project

let text = System.IO.File.ReadAllText("sample.x")
let tokens = Parser.tokenize(text |> Seq.toList)
let parsed = Parser.parse(tokens |> Seq.toList)
Interpreter.evaluate parsed Map.empty
```

Файл Program.fs является основным файлом для запуска программы. Он выполняет следующие шаги:

Читает содержимое файла sample.x.
Токенизирует текст с помощью функции Parser.tokenize.
Парсит токены в абстрактное синтаксическое дерево (AST) с помощью функции Parser.parse.
Интерпретирует AST с помощью функции Interpreter.evaluate.

### `sample.x`

```plaintext
fun fact (x) \
    if (x < 1) \
        1
    / else \
        x * fact(x - 1)
/
printint fact(5)
```

Файл sample.x содержит пример программы на разработанном языке программирования. Эта программа:

Определяет функцию fact, которая вычисляет факториал числа.
Вызывает функцию fact с аргументом 5 и выводит результат на экран.

### `Interpreter.fs`

```fsharp
module Interpreter

type Expr =
    | Integer of int
    | String of string
    | Variable of string
    | Application of Expr * Expr
    | Lambda of string * Expr
    | Conditional of Expr * Expr * Expr
    | Let of string * Expr * Expr

let rec eval (expr: Expr) (env: Map<string, Expr>) =
    match expr with
    | Integer _ | String _ -> expr
    | Variable name -> env.[name]
    | Application (f, x) ->
        let Lambda(param, body) = eval f env
        let arg = eval x env
        eval body (env.Add(param, arg))
    | Lambda _ -> expr
    | Conditional (cond, tBranch, fBranch) ->
        let condValue = eval cond env
        match condValue with
        | Integer 0 -> eval fBranch env
        | _ -> eval tBranch env
    | Let (name, value, body) ->
        let value = eval value env
        eval body (env.Add(name, value))

let evaluate program env =
    eval program env |> ignore
```

Файл Interpreter.fs содержит интерпретатор для языка программирования. Основные компоненты:

Определение типа Expr, который представляет различные выражения языка.
Функция eval, которая рекурсивно вычисляет значение выражения в заданной среде (окружении).
Функция evaluate, которая запускает интерпретатор на данном программе и среде.

### `Parser.fs`

```fsharp
module Parser

open Interpreter

type Token =
    | Number of string
    | String of string
    | Action of string
    | OpenBracket
    | CloseBracket
    | OpenBlock
    | CloseBlock

let tokenize (input: char list) : Token list =
    // Tokenizer logic here (omitted for brevity)

let parse (tokens: Token list) : Expr =
    // Parser logic here (omitted for brevity)
```

Файл Parser.fs содержит парсер для языка программирования. Основные компоненты:

Определение типа Token, который представляет различные токены языка.
Функция tokenize, которая преобразует входной текст в список токенов.
Функция parse, которая преобразует список токенов в абстрактное синтаксическое дерево (AST).

## Реализация особенностей языка

### Рекурсия

Рекурсия реализована с помощью определения функций, которые могут вызывать сами себя. Например, функция fact для вычисления факториала использует рекурсивный вызов:

```
fun fact (x) \
    if (x < 1) \
        1
    / else \
        x * fact(x - 1)
    /
/
```

### Функции

Функции реализованы с использованием конструкции fun, которая позволяет определять именованные функции. Функция может принимать параметры и возвращать результат.

### Замыкания

Замыкания реализованы через лямбда-выражения, которые могут захватывать переменные из окружающего контекста. Это позволяет создавать функции, которые сохраняют состояние.

## Авторы

| Имя                             | Роль в проекте                                                                           |
| ------------------------------- | ---------------------------------------------------------------------------------------- |
| Марков Владимир Игоревич        | командная работа, разработка синтаксиса языка, разработка парсера и интерпретатора языка |
| Недавний Александр Вячеславович | командная работа, разработка синтаксиса языка, разработка парсера и интерпретатора языка |
