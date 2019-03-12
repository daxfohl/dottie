module Types

open Expressions

type LitSpec = StrSpec | IntSpec
type Relation = Contains | ContainedBy

type Spec =
| LitSpec of LitSpec
| FreeSpec of Expr
| FnSpec of Spec * Spec
| ObjSpec of Map<string, Spec>
| FreeObjSpec of Expr * Map<string, Spec>
| FreeFnSpec of Expr * Set<Spec> * Spec
and Constraint =
  { expr: Expr
    relation: Relation
    spec: Spec }

type Specs = Map<Expr, Spec>