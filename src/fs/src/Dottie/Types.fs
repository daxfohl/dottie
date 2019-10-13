module Types

open Expressions

type SLit = SStr | SInt

type S =
| SLit of SLit
| SFree of E
| SFn of S * S * bool
| SObj of Map<string, S>
| SFreeObj of E * Map<string, S>
| SFreeFn of E * Set<S> * S * bool

type Specs = Map<E, S>

type MType =
| Module of E
| ForeignModule of S

type M = string * MType