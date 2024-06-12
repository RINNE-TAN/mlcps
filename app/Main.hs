module Main where

import qualified CPS.Trans as CPS
import qualified Closure.Clo as Clo
import qualified Flat.Hoist as Flat
import ML.Ast (Core (..))
import qualified Machine.Lowering as Machine
import qualified Machine.Pretty as PP
import qualified Spill.Rename as Spill
import Utils.Ident (PrimOp (..), runTrans)

main :: IO ()
main = do
  let ml =
        LetFix
          "sum"
          "x"
          ( If0
              (Var "x")
              (Num 0)
              ( Prim
                  Add
                  [ Var "x",
                    Let
                      "_"
                      (Prim Print [Var "x"])
                      ( App
                          (Var "sum")
                          ( Prim
                              Sub
                              [Var "x", Num 1]
                          )
                      )
                  ]
              )
          )
          (App (Var "sum") (Num 10))
  print ml
  let code = runTrans (CPS.trans ml >>= Clo.cloConv >>= Flat.hoist >>= Spill.rename 3 >>= Machine.lowering)
  let codeDisp = PP.display code
  writeFile "runtime/main.c" (show codeDisp)
