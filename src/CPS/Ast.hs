module CPS.Ast where

import Utils.Ident (Ident, PrimOp)

type K = Ident

type F = Ident

type X = Ident

data CTm
  = LetVal X CVal CTm
  | LetProj X Int X CTm
  | LetCont K X CTm CTm
  | ContApp K X
  | FuncApp F K X
  | Case X K K
  | LetPrim X PrimOp [X] CTm
  | If0 X K K
  | LetFix F K X CTm CTm
  | Halt X
  deriving (Show)

data CVal
  = Unit
  | Num Int
  | Str String
  | Tuple [X]
  | Tag Int X
  | Lam K X CTm
  deriving (Show)