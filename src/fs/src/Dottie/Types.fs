module Types

open System
open Expressions

type EquivalenceSet = EquivalenceSet of Guid

type SLit =
  | SStr
  | SNum
  
type SSub =
  | SSubLit of SLit
  | SSubFree of EquivalenceSet

type SError =
  { message: string }

type SFn =
  { input: SSub
    output: SSub
    isProc: bool }

type SObj =
  { fields: Map<string, SSub> }
  
type S =
  | SLit of SLit
  | SFree of EquivalenceSet
  | SFn of SFn
  | SObj of SObj
  | SError of SError

type MType =
  | Module of E
  | ForeignModule of S

type M = string * MType