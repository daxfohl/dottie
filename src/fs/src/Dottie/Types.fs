module Types

open Expressions
open FSharpx.Collections

type SFree =
  { expr: E }

type EError =
  { message: string }

type S =
| SStr
| SNum
| SFree of SFree
| SFn of SFn
| SObj of SObj
| EError of EError

and SFn =
  { input: S
    output: S 
    isProc: bool
    binding: E }

and SObj =
  { fields: Map<string, S>
    binding: E  }

type Specs = Map<E, S>

type MType =
| Module of E
| ForeignModule of S

type M = string * MType