module Types

open System
open Expressions

type EquivalenceSet = EquivalenceSet of Guid

type SLit =
  | SStr
  | SNum

type SFn =
  { input: EquivalenceSet
    output: EquivalenceSet
    isProc: bool }

type SObj =
  { fields: Map<string, EquivalenceSet> }
  
type S =
  | SLit of SLit
  | SFree of EquivalenceSet
  | SFn of SFn
  | SObj of SObj

type MType =
  | Module of E
  | ForeignModule of S

type M = string * MType