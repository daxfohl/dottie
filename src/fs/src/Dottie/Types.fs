﻿module Types

open Expressions

type LitSpec = StrSpec | IntSpec

type Spec =
| LitSpec of LitSpec
| FreeSpec of Expr
| FnSpec of Spec * Spec
| ObjSpec of Map<string, Spec>

type Specs = Map<Expr, Spec>