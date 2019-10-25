module Tokens

type Token =
| KLet
| KImport
| KDo
| KWith
| KProc
| KFn
| KLiteral
| KModule
| KForeign
| KName of string
| KEquals
| KOpenParen
| KCloseParen
| KOpenBrace
| KCloseBrace
| KSemicolon
| KDot
| KColon
| KArrow
| KNumber of float
| KString of string
| KComment of string
| KError of string

[<ReferenceEquality>]
type PageToken = {
  row: int
  col: int
  len: int
  value: Token
}

let (|K|) pageToken = pageToken.value