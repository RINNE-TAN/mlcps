module Spill.Ast where

import Utils.Ident (Ident, PrimOp)

data Prog = Prog Func [Func]
  deriving (Show)

data Func = Func Ident Int [Bind] Trans
  deriving (Show)

data Bind = Bind Alloc Value
  deriving (Show)

data Value
  = Unit
  | Num Int
  | Str String
  | Proj Int Alloc
  | Tuple [Alloc]
  | Tag Int Alloc
  | Prim PrimOp [Alloc]
  deriving (Show)

data Trans
  = App Alloc [Alloc]
  | If0 Alloc Trans Trans
  | Case Alloc (Alloc, Trans) (Alloc, Trans)
  | Halt Alloc
  deriving (Show)

data Alloc
  = AReg Int
  | TReg Int
  | Label Ident
  | Spill Int
  deriving (Show, Eq, Ord)