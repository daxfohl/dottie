module Types

open System
open Expressions

type EquivalenceSet = EquivalenceSet of Guid

type SError =
  { message: string }

type SLit =
  | SStr
  | SNum

type S =
| SLit of SLit
| SFree of EquivalenceSet
| SFn of SFn
| SObj of SObj
| SError of SError

and SFn =
  { input: S
    output: S
    isProc: bool }

and SObj =
  { fields: Map<string, S> }

type Specs = Map<Guid, S>

type MType =
| Module of E
| ForeignModule of S

type M = string * MType