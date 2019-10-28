module Types

open Expressions

type EError =
  { message: string }

type S =
| SStr
| SNum
| SFree
| SFn of SFn
| SObj of SObj
| EError of EError

and SFn =
  { input: Spec
    output: Spec
    isProc: bool }

and SObj =
  { fields: Map<string, Spec> }

and Spec =
  { spec: S
    expr: E }

type Specs = Map<E, S>

type MType =
| Module of E
| ForeignModule of S

type M = string * MType