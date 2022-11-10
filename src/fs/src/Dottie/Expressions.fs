module Expressions

type [<ReferenceEquality>] EVal =
  { name: string }

type [<ReferenceEquality>] EImport =
  { moduleName: string }

type [<ReferenceEquality>] EError =
  { message: string }

type [<ReferenceEquality>] EBlock =
  { expr: E }

and [<ReferenceEquality>] ELet =
  { identifier: string
    expr: E
    rest: E }

and [<ReferenceEquality>] EObjField =
  { key: string
    value: E }
    
and [<ReferenceEquality>] EObj =
  { fields: EObjField list }
    
and [<ReferenceEquality>] EWith =
  { expr: E
    fields: EObjField list }

and [<ReferenceEquality>] EDo =
  { expr: E }

and [<ReferenceEquality>] EDot =
  { expr: E
    name: string }

and [<ReferenceEquality>] EEval =
  { fnExpr: E
    argExpr: E }

and [<ReferenceEquality>] EFn =
  { argument: string
    expr: E
    isProc: bool }

and [<ReferenceEquality>] E =
  | ENum of float
  | EStr of string
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
