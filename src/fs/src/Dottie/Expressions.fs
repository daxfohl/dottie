module Expressions

open System

type LitExpr =
  | StrExpr of string
  | IntExpr of int

type Expr =
  | LitExpr of LitExpr
  | ValExpr of string
  | LetExpr of string * Expr * Expr
  | EvalExpr of Expr * Expr
  | FnExpr of string * Expr
  | ObjExpr of Map<string, Expr>
  | WithExpr of string * Map<string, Expr>
  | DotExpr of Expr * string
  | ImportExpr of string