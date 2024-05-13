module Machine.Trans where

import qualified Flat.Ast as Flat
import Machine.Ast (Bind (..), Block (..), Func (..), Prog (..), Trans (..), Value (..))
import Utils.Ident (TransM, fresh)

alloc :: Flat.Bind -> TransM [Bind]
alloc (Flat.Bind x v) = return (f v)
  where
    f Flat.Unit = [Bind x Null]
    f (Flat.Num i) = [Bind x (Num i)]
    f (Flat.Str s) = [Bind x (Str s)]
    f (Flat.Proj i y) = [Bind x (Fetch i y)]
    f (Flat.Tuple xs) =
      Bind x (AllocTuple (length xs))
        : (uncurry (Init x) <$> zip [0 ..] xs)
    f (Flat.Tag i y) = [Bind x (AllocTag i), Init x 0 y]
    f (Flat.Prim op ys) = [Bind x (Prim op ys)]

codeGenT :: Flat.Trans -> TransM Block
codeGenT (Flat.App x ys) = do
  z <- fresh "z"
  let bind = Bind z (AllocTuple (length ys)) : (uncurry (Init z) <$> zip [0 ..] ys)
  return (Block bind (App x z))
codeGenT (Flat.If0 x e1 e2) = do
  b1 <- codeGenT e1
  b2 <- codeGenT e2
  return (Block [] (If0 x b1 b2))
codeGenT (Flat.Case x (x1, e1) (x2, e2)) = do
  b1 <- codeGenT e1
  b2 <- codeGenT e2
  return (Block [] (Case x (x1, b1) (x2, b2)))
codeGenT (Flat.Halt x) = do
  return (Block [] (Halt x))

codeGenF :: Flat.Func -> TransM Func
codeGenF (Flat.Func x ys bs e) = do
  z <- fresh "z"
  allocBs <- concat <$> traverse alloc bs
  let fbs = ((\(idx, yi) -> Bind yi (Fetch idx z)) <$> zip [0 ..] ys) ++ allocBs
  Func x z fbs <$> codeGenT e

codeGen :: Flat.Prog -> TransM Prog
codeGen (Flat.Prog m fs) = Prog <$> codeGenF m <*> traverse codeGenF fs
