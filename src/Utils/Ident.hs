module Utils.Ident where

import Control.Monad.State (State, evalState, get, put)

type Ident = String

type TransM a = State Int a

runTrans :: TransM a -> a
runTrans transM = evalState transM 0

fresh :: Ident -> TransM Ident
fresh ident = do
  idx <- get
  put (idx + 1)
  return (ident ++ show idx)

data PrimOp
  = Add
  | Sub
  | Mul
  | Print
  | Int2Str

instance Show PrimOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Print = "Print"
  show Int2Str = "Int2Str"