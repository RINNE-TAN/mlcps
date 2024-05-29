module Reg.Trans where

import Control.Monad.State (State, get, put, runState)
import Data.Foldable (maximumBy, toList)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Sequence as Seq
import Data.Set (Set, empty, fromList, member, singleton, union, (\\))
import qualified Flat.Ast as Flat
import Reg.Ast (Bind (..), Func (..), Prog (..), Reg (..), Trans (..), Value (..))
import qualified Utils.Env as Env
import Utils.Ident (Ident)
import Utils.List (findLastIndex)

rename :: Int -> Flat.Prog -> Prog
rename maxN (Flat.Prog m fs) = Prog (renameF maxN m) (renameF maxN <$> fs)

renameF :: Int -> Flat.Func -> Func
renameF maxN (Flat.Func fname args binds trans) =
  Env.runEnv
    (Func fname nAlloc <$> mapM renameB zipBinds <*> renameT trans)
    (initArg <$> zip [0 ..] args)
  where
    initArg (idx, arg) = (arg, AReg idx)
    live = liveness binds trans
    (regs, nAlloc) = alloc maxN live
    zipBinds = zip binds (snd <$> toList regs)

renameB :: (Flat.Bind, Reg) -> Env.EnvM Ident Reg Bind
renameB (Flat.Bind x v, reg) = do
  nv <- renameV v
  Env.insert x reg
  return (Bind reg nv)

renameT :: Flat.Trans -> Env.EnvM Ident Reg Trans
renameT (Flat.App f arg) = App <$> renameId f <*> mapM renameId arg
renameT (Flat.If0 x t1 t2) = If0 <$> renameId x <*> renameT t1 <*> renameT t2
renameT (Flat.Halt x) = Halt <$> renameId x

renameId :: Ident -> Env.EnvM Ident Reg Reg
renameId ident = do
  reg <- Env.lookup ident
  case reg of
    Just r -> return r
    Nothing -> return (Address ident)

renameV :: Flat.Value -> Env.EnvM Ident Reg Value
renameV Flat.Unit = return Unit
renameV (Flat.Num i) = return (Num i)
renameV (Flat.Str s) = return (Str s)
renameV (Flat.Proj i x) = Proj i <$> renameId x
renameV (Flat.Tuple xs) = Tuple <$> mapM renameId xs
renameV (Flat.Tag i x) = Tag i <$> renameId x
renameV (Flat.Prim op xs) = Prim op <$> mapM renameId xs

freeT :: Flat.Trans -> Set Ident
freeT (Flat.App f arg) = fromList (f : arg)
freeT (Flat.If0 x t1 t2) = singleton x `union` freeT t1 `union` freeT t2
freeT (Flat.Case x (x1, t1) (x2, t2)) = singleton x `union` (freeT t1 \\ singleton x1) `union` (freeT t2 \\ singleton x2)
freeT (Flat.Halt x) = singleton x

freeV :: Flat.Value -> Set Ident
freeV Flat.Unit = empty
freeV (Flat.Num _) = empty
freeV (Flat.Str _) = empty
freeV (Flat.Proj _ x) = singleton x
freeV (Flat.Tuple xs) = fromList xs
freeV (Flat.Tag _ x) = singleton x
freeV (Flat.Prim _ xs) = fromList xs

data Linear = Linear {start :: Int, end :: Int}
  deriving (Show)

liveness :: [Flat.Bind] -> Flat.Trans -> [Linear]
liveness binds trans = f <$> zip [0 ..] binds
  where
    freeInBind (Flat.Bind _ v) = freeV v
    free = (freeInBind <$> binds) ++ [freeT trans]
    f (idx, Flat.Bind x _) = Linear idx (fromMaybe idx (findLastIndex (member x) free))

livenessProg :: Flat.Prog -> [[Linear]]
livenessProg (Flat.Prog m f) = (\(Flat.Func _ _ binds trans) -> liveness binds trans) <$> (m : f)

alloc :: Int -> [Linear] -> (Seq.Seq (Linear, Reg), Int)
alloc maxN linears = runState (allocST maxN linears) 0

allocST :: Int -> [Linear] -> State Int (Seq.Seq (Linear, Reg))
allocST maxN = foldl f (return Seq.empty)
  where
    f :: State Int (Seq.Seq (Linear, Reg)) -> Linear -> State Int (Seq.Seq (Linear, Reg))
    f recordsST cur = do
      records <- recordsST
      let actives = Seq.filter (activeRecord cur) records
      if length actives == maxN
        -- spill in memory
        then
          let (spill, sReg) = spillRecord actives
           in -- spill the last expired record
              do
                -- add alloc idx
                nAlloc <- get
                put (nAlloc + 1)
                if end spill > end cur
                  then
                    return
                      ( Seq.update (start spill) (spill, Alloc nAlloc) records
                          Seq.|> (cur, sReg)
                      )
                  else return (records Seq.|> (cur, Alloc nAlloc))
        -- alloc reg, find first unused reg
        else return (records Seq.|> (cur, TReg (head [x | x <- [0 ..], all (unused x) actives])))

    activeRecord cur (active, reg) = case reg of
      TReg _ | end active >= start cur -> True
      _ -> False
    spillRecord = maximumBy (comparing (end . fst))
    unused x (_, TReg x') = x /= x'
    unused _ _ = True