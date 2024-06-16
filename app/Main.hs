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
          "filter"
          ["num", "f"]
          ( If0
              (App (Var "f") [Var "num"])
              ( App
                  (Var "filter")
                  [ Prim Sub [Var "num", Num 1],
                    Var "f"
                  ]
              )
              (Var "num")
          )
          ( LetFix
              "f"
              ["num"]
              ( LetFix
                  "help"
                  ["i"]
                  ( If0
                      (Var "i")
                      (Num 0)
                      ( If0
                          ( Prim
                              Sub
                              [ Var "num",
                                Prim Mul [Var "i", Var "i"]
                              ]
                          )
                          (Num 1)
                          (App (Var "help") [Prim Sub [Var "i", Num 1]])
                      )
                  )
                  (App (Var "help") [Var "num"])
              )
              (App (Var "filter") [Num 99, Var "f"])
          )
  print ml
  let code = runTrans (CPS.trans ml >>= Clo.cloConv >>= Flat.hoist >>= Spill.rename 3 >>= Machine.lowering)
  let codeDisp = PP.display code
  writeFile "runtime/main.s" (show codeDisp)
