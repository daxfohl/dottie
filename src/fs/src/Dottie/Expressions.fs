﻿module Expressions

open System
open PExpressions

type EStr =
  { str: string }

type ENum =
  { num: float }

type ELit = EStr of EStr | ENum of ENum

type EVal =
  { name: string }

type EImport =
  { moduleName: string }

type EError =
  { message: string }

type E =
  | ELit of ELit
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
    value: E
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
  { argument: EVal
    body: E
    isProc: bool }
    
type Expression =
  { paged: PE
    expr: E }