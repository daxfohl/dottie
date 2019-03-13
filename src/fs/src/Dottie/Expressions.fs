module Expressions

type ELit =
  | EStr of string
  | EInt of int

type E =
  | ELit of ELit
  | EVal of string
  | ELet of string * E * E
  | EEval of E * E
  | EFn of string * E
  | EObj of Map<string, E>
  | EWith of string * Map<string, E>
  | EDot of E * string
  | EImport of string