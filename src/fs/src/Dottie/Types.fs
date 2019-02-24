module Types

open Expressions

type LitSpec = StrSpec | IntSpec
type Relation = Is | Contains | ContainedBy

type Spec =
| LitSpec of LitSpec
| FreeSpec of Expr
| FnSpec of Spec * Spec * Constraint list
| ObjSpec of Map<string, Spec>
| IntersectSpec of Set<Spec>
| UnionSpec of Set<Spec>
and Constraint =
  { expr: Expr
    relation: Relation
    spec: Spec }

type Specs = Map<Expr, Spec>