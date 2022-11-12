module Tokens

type Token =
    | KNumber of float
    | KString of string
    | KIdentifier of string
    | KImport
    | KFn
    | KProc
    | KLet
    | KDo
    | KWith
    | KEquals
    | KLiteral
    | KModule
    | KForeign
    | KOpenParen
    | KCloseParen
    | KOpenBrace
    | KCloseBrace
    | KDot
    | KColon
    | KArrow
    | KSemicolon
    | KComment of string
    | KError of string

[<ReferenceEquality>]
type PageToken =
    { row: int
      col: int
      len: int
      value: Token }

let (|K|) pageToken = pageToken.value

let isIgnorable =
    function
    | K KSemicolon
    | K (KComment _)
    | K (KError _) -> true
    | _ -> false

let (|Ignorable|Active|) token =
    if isIgnorable token then
        Ignorable
    else
        Active
