module Types

open Expressions

type LitSpec = StrSpec | IntSpec

type Spec =
  | LitSpec of LitSpec
  | FreeSpec of Expr
  | FnSpec of Spec * Spec

type Specs = Map<Expr, Spec>