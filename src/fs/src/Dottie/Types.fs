module Types

open Expressions
open FSharpx.Collections

type SFree =
  { expr: E }

type S =
| SStr
| SNum
| SFree of SFree
| SFn of SFn
| SObj of SObj
| SFreeObj of SFreeObj
| SFreeFn of SFreeFn

and SFn =
  { argsType: S
    resultType: S 
    isProc: bool }

and SObj =
  { fields: Map<string, S> }

and SFreeObj =
  { expr: E
    fields: Map<string, S> }
    
and SFreeFn =
  { expr: E
    args: Map<string, S>
    resultType: S
    isProc: bool }

type Specs = PersistentHashMap<E, S>

type MType =
| Module of E
| ForeignModule of S

type M = string * MType