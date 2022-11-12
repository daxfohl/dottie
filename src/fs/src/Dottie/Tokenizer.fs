module Tokenizer

open System
open System.Collections.Generic
open System.Linq
open System.Text.RegularExpressions
open Tokens

let insertSemicolon (tokens: PageToken list) : PageToken list =
    let rec insertSemicolon (newTokens: PageToken list) =
        function
        | [] -> List.rev newTokens
        | x :: t ->
            let fakeSemi =
                { row = x.row
                  len = 0
                  col = x.col + x.len
                  value = KSemicolon }

            match x.value with
            | KCloseBrace -> insertSemicolon (x :: fakeSemi :: newTokens) t // For consistency finding end of expr in { a: 1; b: 2 }, it is  { a: 1; b: 2; }
            | KWith -> insertSemicolon (x :: fakeSemi :: newTokens) t // To find end of expr in { <expr> with ... }, it's { <expr>; with ... }
            | _ -> insertSemicolon (x :: newTokens) t

    insertSemicolon [] tokens

let removeDuplicateSemicolons (tokens: PageToken list) =
    let rec removeDuplicateSemicolons (newTokens: PageToken list) =
        function
        | [] -> List.rev newTokens
        | (K KSemicolon as x) :: K KSemicolon :: t -> removeDuplicateSemicolons (x :: newTokens) t
        | (K KOpenBrace as x) :: K KSemicolon :: (K KCloseBrace as z) :: t ->
            removeDuplicateSemicolons (z :: x :: newTokens) t
        | x :: t -> removeDuplicateSemicolons (x :: newTokens) t

    removeDuplicateSemicolons [] tokens

type State =
    | Empty // empty, in name, in symbol
    | InNumber of bool
    | InString of bool
    | InComment
    | InWhitespace
    | InWord
    | InSymbol

let createToken (currentToken: char seq, state: State, lineNumber: int, charNumber: int) =
    let tokenStr = String(currentToken.ToArray())

    let token =
        match state with
        | InWord ->
            match tokenStr with
            | "let" -> KLet
            | "import" -> KImport
            | "foreign" -> KForeign
            | "do" -> KDo
            | "with" -> KWith
            | "proc" -> KProc
            | "fn" -> KFn
            | "literal" -> KLiteral
            | _ -> KIdentifier tokenStr
        | InSymbol ->
            match tokenStr with
            | "=" -> KEquals
            | "(" -> KOpenParen
            | ")" -> KCloseParen
            | "{" -> KOpenBrace
            | "}" -> KCloseBrace
            | ";"
            | "," -> KSemicolon
            | "." -> KDot
            | ":" -> KColon
            | "->" -> KArrow
            | _ -> KError tokenStr
        | InNumber _ -> KNumber ^% Double.Parse(tokenStr)
        | InString true -> KError tokenStr
        | InString false ->
            let rest = tokenStr.Substring(1)

            if rest.Length = 0 || rest.[rest.Length - 1] <> '\"' then
                KError tokenStr
            else
                KString
                ^% Regex.Unescape(rest.Substring(0, rest.Length - 1))
        | InComment -> KComment tokenStr
        | InWhitespace
        | Empty -> failwith "Cannot create token here"

    { row = lineNumber
      col = charNumber - tokenStr.Length
      len = tokenStr.Length
      value = token }

let getNextState =
    function
    | None, _ -> Empty
    | Some c, _ when Char.IsWhiteSpace(c) -> InWhitespace
    | Some c, _ when c = '"' -> InString false
    | Some c, Some c1 when c = '/' && c1 = '/' -> InComment
    | Some c, _ when Char.IsDigit(c) -> InNumber false
    | Some c, Some c1 when c = '-' && Char.IsDigit c1 -> InNumber false
    | Some c, _ when Char.IsLetter(c) -> InWord
    | Some _, _ -> InSymbol

let addFinalSemicolon (tokens: Stack<PageToken>) =
    if tokens.Count <> 0 then
        let lastToken = tokens.Peek()

        match lastToken.value with
        | KIdentifier _
        | KCloseBrace
        | KNumber _
        | KString _ ->
            tokens.Push(
                { row = lastToken.row
                  len = 0
                  col = lastToken.col + lastToken.len
                  value = KSemicolon }
            )
        | _ -> ()

let complete
    (
        currentToken: char IList,
        tokens: PageToken Stack,
        charNumber: int,
        lineNumber: int,
        cNext: char option,
        cAfter: char option,
        state: State ref
    ) =
    if !state <> Empty && !state <> InWhitespace then
        tokens.Push(createToken (currentToken, !state, lineNumber, charNumber))

    currentToken.Clear()

    match cNext with
    | Some c -> currentToken.Add(c)
    | _ -> ()

    state := getNextState (cNext, cAfter)

let tokenizeLine (line: string, lineNumber: int) : PageToken list =
    let tokens = Stack<PageToken>()
    let currentToken = List<char>()
    let state = ref Empty

    for charId = 0 to line.Length - 1 do
        let c = line.[charId]

        let cAfter =
            if line.Length > charId + 1 then
                Some line.[charId + 1]
            else
                None

        let completeAndLoad =
            fun () -> complete (currentToken, tokens, charId, lineNumber, Some c, cAfter, state)

        let completeOnly =
            fun () -> complete (currentToken, tokens, charId, lineNumber, None, None, state)

        match !state with
        | Empty -> completeAndLoad ()
        | InWhitespace -> completeAndLoad ()
        | InString false ->
            currentToken.Add(c)

            if c = '\\' then state := InString true
            elif c = '\"' then completeOnly ()
        | InString true ->
            currentToken.Add(c)
            state := InString false
        | InComment -> currentToken.Add(c)
        | InNumber false ->
            if Char.IsNumber(c) || c = '.' then
                currentToken.Add(c)
                if c = '.' then state := InNumber true
            else
                completeAndLoad ()
        | InNumber true ->
            if Char.IsNumber(c) then
                currentToken.Add(c)
            else
                completeAndLoad ()
        | InWord ->
            if Char.IsLetterOrDigit(c) then
                currentToken.Add(c)
            else
                completeAndLoad ()
        | InSymbol ->
            if currentToken.[0] = '-' && c = '>' then
                currentToken.Add(c)
            else
                completeAndLoad ()

    complete (currentToken, tokens, line.Length, lineNumber, None, None, state)

    let commentOpt =
        match tokens.Count with
        | 0 -> None
        | _ ->
            match tokens.Peek().value with
            | KComment _ -> Some ^% tokens.Pop()
            | _ -> None

    addFinalSemicolon (tokens)

    match commentOpt with
    | Some comment -> tokens.Push(comment)
    | None -> ()

    tokens |> List.ofSeq |> List.rev


let tokenize (file: string) : PageToken list =
    let lines = Regex.Split(file, "\r\n|\r|\n")
    let tokens = List<PageToken>()

    for lineId = 0 to lines.Length - 1 do
        let line = lines.[lineId]
        let lineTokens = tokenizeLine (line, lineId)
        tokens.AddRange(lineTokens)

    let tokenList = List.ofSeq tokens

    tokenList
    |> insertSemicolon
    |> removeDuplicateSemicolons
