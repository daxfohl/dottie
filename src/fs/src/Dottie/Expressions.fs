module Expressions

type E =
    | ENum of value: float
    | EStr of value: string
    | EVal of name: string
    | EBlock of expr: E
    | ELet of identifier: string * expr: E * rest: E
    | EEval of fnExpr: E * argExpr: E
    | EFn of argument: string * expr: E * isProc: bool
    | EObj of fields: Map<string, E>
    | EWith of expr: E * fields: Map<string, E>
    | EDot of expr: E * name: string
    | EDo of expr: E
    | EImport of moduleName: string
    | EError of message: string
