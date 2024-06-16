module Closure.Ast where

import Utils.Ident (Ident, PrimOp)

type K = Ident

type F = Ident

type X = Ident

type Env = Ident

data CloTm
  = LetVal X CloVal CloTm
  | LetProj X Int X CloTm
  | LetCont K Env X CloTm CloTm
  | ContApp K Env [X]
  | FuncApp F Env K [X]
  | Case X (X, CloTm) (X, CloTm)
  | LetPrim X PrimOp [X] CloTm
  | If0 X CloTm CloTm
  | LetFix F Env K [X] CloTm CloTm
  | Halt X
  deriving (Show)

data CloVal
  = Unit
  | Num Int
  | Str String
  | Tuple [X]
  | Tag Int X
  | Lam Env K X CloTm
  deriving (Show)