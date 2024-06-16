module ML.Ast where

import Utils.Ident (Ident, PrimOp)

data Core
  = Var String
  | Unit
  | Num Int
  | Str String
  | Lam Ident Core
  | App Core [Core]
  | Tuple [Core]
  | Proj Int Core
  | Tag Int Core
  | Case Core (Ident, Core) (Ident, Core)
  | Prim PrimOp [Core]
  | Let Ident Core Core
  | If0 Core Core Core
  | LetFix Ident [Ident] Core Core
  deriving (Show)
