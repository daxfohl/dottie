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
| SFreeObj of SFreeObj
| SFreeFn of SFreeFn
| EError of EError

and SFn =
  { input: S
    output: S 
    isProc: bool }

and SObj =
  { fields: Map<string, S> }

and SFreeObj =
  { expr: E
    fields: Map<string, S> }
    
and SFreeFn =
  { expr: E
    args: Map<string, S>
    output: S
    isProc: bool }

type Specs = PersistentHashMap<E, S>

type MType =
| Module of E
| ForeignModule of S

type M = string * MType