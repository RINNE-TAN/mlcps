module Main where

import qualified CPS.Trans as CPS
import qualified Closure.Trans as Closure
import qualified Flat.Trans as Flat
import ML.Ast (Core (..))
import qualified Reg.Pretty as PP
import qualified Reg.Trans as Reg
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
  let code = runTrans (CPS.trans ml >>= Closure.convert >>= Flat.hoist)
  let progRename = Reg.rename 3 code
  let codeDisp = PP.display progRename
  writeFile "runtime/main.c" (show codeDisp)
