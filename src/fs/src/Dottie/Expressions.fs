module Expressions

open System
open PExpressions

type EStr =
  { str: string }

type ENum =
  { num: float }

type EVal =
  { id: Guid
    name: string }

type EImport =
  { moduleName: string }

type EError =
  { message: string }

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

and EBlock =
  { expr: E }

and ELet =
  { identifier: EVal
    expr: E
    rest: E }

and EObjField =
  { key: string
    value: E }
    
and EObj =
  { fields: EObjField list }
    
and EWith =
  { expr: E
    fields: EObjField list }

and EDo =
  { expr: E }

and EDot =
  { expr: E
    name: string }

and EEval =
  { fnExpr: E
    argExpr: E }

and EFn =
  { identifier: EVal
    expr: E
    isProc: bool }
    
type Expression =
  { paged: PE
    expr: E }