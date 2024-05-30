module Closure.Clo where

import qualified CPS.Ast as CPS
import Closure.Ast (CloTm (..), CloVal (..))
import Data.Set (Set, empty, fromList, singleton, toList, union, (\\))
import Utils.Ident (Ident, TransM, fresh)

freeF :: CPS.CTm -> Set Ident
freeF (CPS.LetVal x v k) = (freeF k \\ singleton x) `union` freeH v
freeF (CPS.LetProj x _ y k) = (freeF k \\ singleton x) `union` singleton y
freeF (CPS.LetCont k x k1 k2) = (freeF k1 \\ singleton x) `union` (freeF k2 \\ singleton k)
freeF (CPS.ContApp k x) = fromList [k, x]
freeF (CPS.FuncApp f k x) = fromList [f, k, x]
freeF (CPS.Case x k1 k2) = fromList [x, k1, k2]
freeF (CPS.LetPrim x _ ys k) = (freeF k \\ singleton x) `union` fromList ys
freeF (CPS.If0 x k1 k2) = fromList [x, k1, k2]
freeF (CPS.LetFix f k x k1 k2) = (freeF k1 \\ fromList [f, k, x]) `union` (freeF k2 \\ singleton f)
freeF (CPS.Halt x) = singleton x

freeH :: CPS.CVal -> Set Ident
freeH CPS.Unit = empty
freeH (CPS.Num _) = empty
freeH (CPS.Str _) = empty
freeH (CPS.Tuple xs) = fromList xs
freeH (CPS.Tag _ x) = singleton x
freeH (CPS.Lam k x e) = freeF e \\ fromList [k, x]

cloConv :: CPS.CTm -> TransM CloTm
cloConv (CPS.LetProj x i y k) = LetProj x i y <$> cloConv k
cloConv (CPS.LetCont k x k1 k2) = do
  let ys = toList (freeF k1 \\ singleton x)
  kCode <- fresh "kCode"
  env <- fresh "env"
  env1 <- fresh "env"
  clok1 <- cloConv k1
  clok2 <- cloConv k2
  return
    ( LetCont
        kCode
        env
        x
        (foldr (\(idx, yi) acc -> LetProj yi idx env acc) clok1 (zip [0 ..] ys))
        ( LetVal
            env1
            (Tuple ys)
            ( LetVal
                k
                (Tuple [kCode, env1])
                clok2
            )
        )
    )
cloConv (CPS.ContApp k x) = do
  kCode <- fresh "kCode"
  env <- fresh "env"
  return
    ( LetProj
        kCode
        0
        k
        ( LetProj
            env
            1
            k
            (ContApp kCode env x)
        )
    )
cloConv (CPS.FuncApp f k x) = do
  fCode <- fresh "fCode"
  env <- fresh "env"
  return
    ( LetProj
        fCode
        0
        f
        ( LetProj
            env
            1
            f
            (FuncApp fCode env k x)
        )
    )
cloConv (CPS.Case x k1 k2) = do
  x1 <- fresh "x"
  x2 <- fresh "x"
  clokx1 <- cloConv (CPS.ContApp k1 x1)
  clokx2 <- cloConv (CPS.ContApp k2 x2)
  return (Case x (k1, clokx1) (k2, clokx2))
cloConv (CPS.LetPrim x op ys k) = LetPrim x op ys <$> cloConv k
cloConv (CPS.If0 x k1 k2) = do
  x1 <- fresh "x"
  clokx1 <- cloConv (CPS.ContApp k1 x1)
  clokx2 <- cloConv (CPS.ContApp k2 x1)
  return (LetVal x1 Unit (If0 x clokx1 clokx2))
cloConv (CPS.LetFix f k x k1 k2) = do
  let ys = toList (freeF k1 \\ fromList [f, k, x])
  fCode <- fresh "fCode"
  env <- fresh "env"
  env1 <- fresh "env"
  clok1 <- cloConv k1
  clok2 <- cloConv k2
  return
    ( LetFix
        fCode
        env
        k
        x
        ( LetVal
            f
            (Tuple [fCode, env])
            (foldr (\(idx, yi) acc -> LetProj yi idx env acc) clok1 (zip [0 ..] ys))
        )
        ( LetVal
            env1
            (Tuple ys)
            ( LetVal
                f
                (Tuple [fCode, env1])
                clok2
            )
        )
    )
cloConv (CPS.LetVal x CPS.Unit k) = LetVal x Unit <$> cloConv k
cloConv (CPS.LetVal x (CPS.Num i) k) = LetVal x (Num i) <$> cloConv k
cloConv (CPS.LetVal x (CPS.Str s) k) = LetVal x (Str s) <$> cloConv k
cloConv (CPS.LetVal x (CPS.Tuple xs) k) = LetVal x (Tuple xs) <$> cloConv k
cloConv (CPS.LetVal x (CPS.Tag i y) k) = LetVal x (Tag i y) <$> cloConv k
cloConv (CPS.LetVal x (CPS.Lam k z k1) k2) = do
  let ys = toList (freeF k1 \\ fromList [k, z])
  xCode <- fresh "xCode"
  env <- fresh "env"
  env1 <- fresh "env"
  clok1 <- cloConv k1
  clok2 <- cloConv k2
  return
    ( LetVal
        xCode
        ( Lam
            env
            k
            z
            (foldr (\(idx, yi) acc -> LetProj yi idx env acc) clok1 (zip [0 ..] ys))
        )
        ( LetVal
            env1
            (Tuple ys)
            ( LetVal
                x
                (Tuple [xCode, env1])
                clok2
            )
        )
    )
cloConv (CPS.Halt x) = return (Halt x)