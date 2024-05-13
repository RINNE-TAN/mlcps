module CPS.Trans where

import CPS.Ast (CTm (..), CVal (..))
import qualified ML.Ast as ML
import Utils.Ident (Ident, TransM, fresh)

trans :: ML.Core -> TransM CTm
trans ml = transCont ml (return . Halt)

traverseC :: (c -> (a -> r) -> r) -> [c] -> ([a] -> r) -> r
traverseC _ [] cont = cont []
traverseC f (e : es) cont = f e (\z -> traverseC f es (\zs -> cont (z : zs)))

transCont :: ML.Core -> (Ident -> TransM CTm) -> TransM CTm
transCont ml cont = transContH ml
  where
    transContH (ML.Var x) = cont x
    transContH ML.Unit = do
      x <- fresh "x"
      LetVal x Unit <$> cont x
    transContH (ML.Num i) = do
      x <- fresh "x"
      LetVal x (Num i) <$> cont x
    transContH (ML.Str s) = do
      x <- fresh "x"
      LetVal x (Str s) <$> cont x
    transContH (ML.App e1 e2) =
      transCont
        e1
        ( \z1 ->
            transCont
              e2
              ( \z2 -> do
                  k <- fresh "k"
                  x <- fresh "x"
                  kx <- cont x
                  return (LetCont k x kx (FuncApp z1 k z2))
              )
        )
    transContH (ML.Tuple es) =
      traverseC
        transCont
        es
        ( \zs -> do
            x <- fresh "x"
            LetVal x (Tuple zs) <$> cont x
        )
    transContH (ML.Tag i e) =
      transCont
        e
        ( \z -> do
            x <- fresh "x"
            LetVal x (Tag i z) <$> cont x
        )
    transContH (ML.Proj i e) =
      transCont
        e
        ( \z -> do
            x <- fresh "x"
            LetProj x i z <$> cont x
        )
    transContH (ML.Lam x e) = do
      f <- fresh "f"
      k <- fresh "k"
      z <- transCont e (return . ContApp k)
      LetVal f (Lam k x z) <$> cont f
    transContH (ML.Let x e1 e2) = do
      j <- fresh "j"
      LetCont j x <$> transCont e2 cont <*> transCont e1 (return . ContApp j)
    transContH (ML.Case e (x1, e1) (x2, e2)) =
      transCont
        e
        ( \z -> do
            x0 <- fresh "x"
            k0 <- fresh "k"
            k1 <- fresh "k"
            k2 <- fresh "k"
            kx0 <- cont x0
            z1 <- transCont e1 (return . ContApp k0)
            z2 <- transCont e2 (return . ContApp k0)
            return
              ( LetCont
                  k0
                  x0
                  kx0
                  ( LetCont
                      k1
                      x1
                      z1
                      ( LetCont
                          k2
                          x2
                          z2
                          (Case z k1 k2)
                      )
                  )
              )
        )
    transContH (ML.Prim op es) =
      traverseC
        transCont
        es
        ( \zs -> do
            x <- fresh "x"
            LetPrim x op zs <$> cont x
        )
    transContH (ML.LetFix f x e1 e2) = do
      k <- fresh "k"
      LetFix f k x <$> transCont e1 (return . ContApp k) <*> transCont e2 cont
    transContH (ML.If0 e e1 e2) =
      transCont
        e
        ( \z -> do
            x0 <- fresh "x"
            x1 <- fresh "x"
            x2 <- fresh "x"
            k0 <- fresh "k"
            k1 <- fresh "k"
            k2 <- fresh "k"
            kx0 <- cont x0
            z1 <- transCont e1 (return . ContApp k0)
            z2 <- transCont e2 (return . ContApp k0)
            return
              ( LetCont
                  k0
                  x0
                  kx0
                  ( LetCont
                      k1
                      x1
                      z1
                      ( LetCont
                          k2
                          x2
                          z2
                          (If0 z k1 k2)
                      )
                  )
              )
        )
