module Flat.Hoist where

import qualified Closure.Ast as Cl
import Flat.Ast (Bind (..), Func (..), Prog (..), Trans (..), Value (..))
import Utils.Ident (TransM, fresh)

type HoistResult = ([Func], [Bind], Trans)

hoist :: Cl.CloTm -> TransM Prog
hoist cl = do
  main <- fresh "main"
  (f, b, e) <- hoistExpr cl
  return (Prog (Func main [] b e) f)

hoistV :: Cl.CloVal -> TransM Value
hoistV Cl.Unit = return Unit
hoistV (Cl.Num i) = return (Num i)
hoistV (Cl.Str s) = return (Str s)
hoistV (Cl.Tuple xs) = return (Tuple xs)
hoistV (Cl.Tag i x) = return (Tag i x)
hoistV _ = undefined

hoistExpr :: Cl.CloTm -> TransM HoistResult
hoistExpr (Cl.LetVal x (Cl.Lam env k z k1) k2) = do
  (f1, b1, e1) <- hoistExpr k1
  (f2, b2, e2) <- hoistExpr k2
  let fx = Func x [env, k, z] b1 e1
  return (f1 ++ f2 ++ [fx], b2, e2)
hoistExpr (Cl.LetVal x v k) = do
  (f, b, e) <- hoistExpr k
  vH <- hoistV v
  return (f, Bind x vH : b, e)
hoistExpr (Cl.LetProj x i y k) = do
  (f, b, e) <- hoistExpr k
  return (f, Bind x (Proj i y) : b, e)
hoistExpr (Cl.LetCont k env x k1 k2) = do
  (f1, b1, e1) <- hoistExpr k1
  (f2, b2, e2) <- hoistExpr k2
  let fk = Func k [env, x] b1 e1
  return (f1 ++ f2 ++ [fk], b2, e2)
hoistExpr (Cl.ContApp k env x) = do
  return ([], [], App k [env, x])
hoistExpr (Cl.FuncApp f env k x) = do
  return ([], [], App f [env, k, x])
hoistExpr (Cl.Case x (x1, k1) (x2, k2)) = do
  (f1, b1, e1) <- hoistExpr k1
  (f2, b2, e2) <- hoistExpr k2
  return (f1 ++ f2, b1 ++ b2, Case x (x1, e1) (x2, e2))
hoistExpr (Cl.LetPrim x op ys k) = do
  (f, b, e) <- hoistExpr k
  return (f, Bind x (Prim op ys) : b, e)
hoistExpr (Cl.If0 x k1 k2) = do
  (f1, b1, e1) <- hoistExpr k1
  (f2, b2, e2) <- hoistExpr k2
  return (f1 ++ f2, b1 ++ b2, If0 x e1 e2)
hoistExpr (Cl.LetFix f env k x k1 k2) = do
  (f1, b1, e1) <- hoistExpr k1
  (f2, b2, e2) <- hoistExpr k2
  let ff = Func f [env, k, x] b1 e1
  return (f1 ++ f2 ++ [ff], b2, e2)
hoistExpr (Cl.Halt x) = return ([], [], Halt x)