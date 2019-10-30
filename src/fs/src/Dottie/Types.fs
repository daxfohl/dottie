module Types

open System
open Expressions

type SError =
  { message: string }

type SLit =
  | SStr
  | SNum

type S =
| SLit of SLit
| SFree
| SFn of SFn
| SObj of SObj
| SError of SError

and SFn =
  { input: Spec
    output: Spec
    isProc: bool }

and SObj =
  { fields: Map<string, Spec> }

and Spec =
  { spec: S
    expr: E }

type Specs = Map<Guid, S>

type MType =
| Module of E
| ForeignModule of S

type M = string * MType