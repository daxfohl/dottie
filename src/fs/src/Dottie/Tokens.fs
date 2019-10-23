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
| KOpenCurly
| KClosedCurly
| KSemicolon
| KDot
| KColon
| KArrow
| KNumber of float
| KString of string
| KComment of string
| KError of string

type PageToken = {
  row: int
  col: int
  len: int
  value: Token
}