module Types

open Expressions

type SLit = SStr | SInt
type Relation = Contains | ContainedBy

type S =
| SLit of SLit
| SFree of E
| SFn of S * S
| SObj of Map<string, S>
| SFreeObj of E * Map<string, S>
| SFreeFn of E * Set<S> * S
and Constraint =
  { expr: E
    relation: Relation
    spec: S }

type Specs = Map<E, S>