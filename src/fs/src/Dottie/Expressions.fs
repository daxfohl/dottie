module Expressions

open Tokens

[<ReferenceEquality>]
type EStr =
  { str: string
    token: PageToken }

[<ReferenceEquality>]
type ENum =
  { num: float
    token: PageToken }

[<ReferenceEquality>]
type EVal =
  { name: string
    token: PageToken }

[<ReferenceEquality>]
type EImport =
  { importToken: PageToken
    moduleName: string
    nameToken: PageToken }

[<ReferenceEquality>]
type EError =
  { message: string
    found: PageToken list }

[<ReferenceEquality>]
type E =
  | EStr of EStr
  | ENum of ENum
  | EVal of EVal
  | EBlock of EBlock
  | ELet of ELet
  | EEval of EEval
  | EFn of EFn
  | EObj of EObj
  | EWith of EWith
  | EDot of EDot
  | EDo of EDo
  | EImport of EImport
  | EError of EError

and [<ReferenceEquality>] EBlock =
  { openToken: PageToken
    expr: E
    closeToken: PageToken }

and [<ReferenceEquality>] ELet =
  { letToken: PageToken 
    name: string
    nameToken: PageToken
    equalsToken: PageToken
    expr: E
    rest: E }

and [<ReferenceEquality>] EObjField =
  { key: string
    keyToken: PageToken
    colonToken: PageToken
    value: E }
    
and [<ReferenceEquality>] EObj =
  { openToken: PageToken
    fields: EObjField list
    closeToken: PageToken }
    
and [<ReferenceEquality>] EWith =
  { openToken: PageToken
    expr: E
    withToken: PageToken
    fields: EObjField list
    closeToken: PageToken }

and [<ReferenceEquality>] EDo =
  { doToken: PageToken
    expr: E }

and [<ReferenceEquality>] EDot =
  { expr: E
    dotToken: PageToken
    name: string
    nameToken: PageToken }

and [<ReferenceEquality>] EEval =
  { fnExpr: E
    argExpr: E }

and [<ReferenceEquality>] EFn =
  { fnToken: PageToken
    name: string
    nameToken: PageToken
    arrowToken: PageToken
    argExpr: E
    isProc: bool }
