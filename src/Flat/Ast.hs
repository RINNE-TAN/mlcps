module Flat.Ast where

import Utils.Ident (Ident, PrimOp)

data Prog = Prog Func [Func]
  deriving (Show)

data Func = Func Ident [Ident] [Bind] Trans
  deriving (Show)

data Bind = Bind Ident Value
  deriving (Show)

data Value
  = Unit
  | Num Int
  | Str String
  | Proj Int Ident
  | Tuple [Ident]
  | Tag Int Ident
  | Prim PrimOp [Ident]
  deriving (Show)

data Trans
  = App Ident [Ident]
  | If0 Ident Trans Trans
  | Case Ident (Ident, Trans) (Ident, Trans)
  | Halt Ident
  deriving (Show)