module ML.Ast where

import Utils.Ident (PrimOp)

data Core
  = Var String
  | Unit
  | Num Int
  | Str String
  | Lam String Core
  | App Core Core
  | Tuple [Core]
  | Proj Int Core
  | Tag Int Core
  | Case Core (String, Core) (String, Core)
  | Prim PrimOp [Core]
  | Let String Core Core
  | If0 Core Core Core
  | LetFix String String Core Core
  deriving (Show)
