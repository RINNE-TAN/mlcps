module Reg.Ast where

import Utils.Ident (Ident, PrimOp)

data Prog = Prog Func [Func]
  deriving (Show)

data Func = Func Ident Int [Bind] Trans
  deriving (Show)

data Bind = Bind Reg Value
  deriving (Show)

data Value
  = Unit
  | Num Int
  | Str String
  | Proj Int Reg
  | Tuple [Reg]
  | Tag Int Reg
  | Prim PrimOp [Reg]
  deriving (Show)

data Trans
  = App Reg [Reg]
  | If0 Reg Trans Trans
  | Case Reg (Reg, Trans) (Reg, Trans)
  | Halt Reg
  deriving (Show)

data Reg
  = AReg Int
  | TReg Int
  | Address Ident
  | Alloc Int
  deriving (Show, Eq, Ord)