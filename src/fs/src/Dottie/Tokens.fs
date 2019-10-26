module Tokens

type Token =
| KNumber of float // x-
| KString of string // x-
| KIdentifier of string // x-
| KImport // x-
| KFn // x-
| KProc // x-
| KLet // x-
| KDo // x-
| KWith // x
| KEquals // x
| KLiteral
| KModule
| KForeign
| KOpenParen // x-
| KCloseParen // x
| KOpenBrace // x-
| KCloseBrace // x
| KDot // x +
| KColon // x
| KArrow // x
| KSemicolon // x-
| KComment of string // x-
| KError of string // x-

[<ReferenceEquality>]
type PageToken = {
  row: int
  col: int
  len: int
  value: Token
}

let (|K|) pageToken = pageToken.value

let isIgnorable =
  function
  | K KSemicolon
  | K (KComment _)
  | K (KError _) -> true
  | _ -> false

let (|Ignorable|Active|) token = if isIgnorable token then Ignorable else Active