module PExpressions

open Tokens

type PEStr =
  { str: string
    token: PageToken }

type PENum =
  { num: float
    token: PageToken }

type PEVal =
  { name: string
    token: PageToken }

type PEImport =
  { importToken: PageToken
    moduleName: string
    nameToken: PageToken }

type PEError =
  { message: string
    found: PageToken list }

type PE =
  | PEStr of PEStr
  | PENum of PENum
  | PEVal of PEVal
  | PEBlock of PEBlock
  | PELet of PELet
  | PEEval of PEEval
  | PEFn of PEFn
  | PEObj of PEObj
  | PEWith of PEWith
  | PEDot of PEDot
  | PEDo of PEDo
  | PEImport of PEImport
  | PEError of PEError

and PEBlock =
  { openToken: PageToken
    expr: PE
    closeToken: PageToken }

and PELet =
  { letToken: PageToken 
    name: string
    nameToken: PageToken
    equalsToken: PageToken
    expr: PE
    rest: PE }

and PEObjField =
  { key: string
    keyToken: PageToken
    colonToken: PageToken
    value: PE }
    
and PEObj =
  { openToken: PageToken
    fields: PEObjField list
    closeToken: PageToken }
    
and PEWith =
  { openToken: PageToken
    expr: PE
    withToken: PageToken
    fields: PEObjField list
    closeToken: PageToken }

and PEDo =
  { doToken: PageToken
    expr: PE }

and PEDot =
  { expr: PE
    dotToken: PageToken
    name: string
    nameToken: PageToken }

and PEEval =
  { fnExpr: PE
    argExpr: PE }

and PEFn =
  { fnToken: PageToken
    name: string
    nameToken: PageToken
    arrowToken: PageToken
    expr: PE
    isProc: bool }