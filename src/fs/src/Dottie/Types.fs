module Types

open Expressions
open FSharpx.Collections

type SLit = SStr | SInt

type S =
| SLit of SLit
| SFree of E
| SFn of S * S * bool
| SObj of Map<string, S>
| SFreeObj of E * Map<string, S>
| SFreeFn of E * PersistentHashMap<S, unit> * S * bool

type Specs = PersistentHashMap<E, S>

type MType =
| Module of E
| ForeignModule of S

type M = string * MType