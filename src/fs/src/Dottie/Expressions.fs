module Expressions

type ELit =
  | EStr of string
  | EInt of int

type E =
  | ELit of ELit
  | EVal of string
  | ELet of string * E * E
  | EEval of E * E
  | EFn of string * E * bool
  | EObj of Map<string, E>
  | EWith of E * Map<string, E>
  | EDot of E * string
  | EDo of E
  | EImport of string