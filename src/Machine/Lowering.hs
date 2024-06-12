module Machine.Lowering where

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Foldable (traverse_)
import Machine.Ast (Block (..), Inst (..), LastInst (..), Prog (..), Reg (..))
import qualified Spill.Ast as Spill
import Utils.Ident (PrimOp (..), TransM, fresh)

type InstW = Writer [Inst]

lowering :: Spill.Prog -> TransM Prog
lowering (Spill.Prog m fs) = do
  (mtb, mb) <- lFunc m
  fsb <- traverse lFunc fs
  return (Prog mb (mtb ++ concatMap join fsb))
  where
    join (tb, b) = b : tb

lFunc :: Spill.Func -> TransM ([Block], Block)
lFunc (Spill.Func f nAlloc binds trans) = do
  transW <- lTrans trans
  let wBody = allocInsts S0 nAlloc >> traverse_ lBind binds >> transW
  let ((blocks, lastInst), insts) = runWriter wBody
  return (blocks, Block f insts lastInst)

lTrans :: Spill.Trans -> TransM (InstW ([Block], LastInst))
lTrans (Spill.App f args) =
  return
    ( do
        let saveAReg (i, alloc) = do
              reg <- lRand1 alloc
              tell [AddImm (AReg i) reg 0]
        traverse_ saveAReg (zip [0 ..] args)
        fReg <- lRand1 f
        return ([], Jump fReg)
    )
lTrans (Spill.If0 x t1 t2) = do
  b1 <- fresh "block"
  b2 <- fresh "block"
  t1W <- lTrans t1
  t2W <- lTrans t2
  let ((blocks1, lastInst1), insts1) = runWriter t1W
  let ((blocks2, lastInst2), insts2) = runWriter t2W
  return
    ( do
        reg <- lRand1 x
        return ([Block b1 insts1 lastInst1, Block b2 insts2 lastInst2] ++ blocks1 ++ blocks2, If0 reg b1 b2)
    )
lTrans (Spill.Halt x) = do
  return
    ( do
        reg <- lRand1 x
        tell [AddImm (AReg 0) reg 0]
        return ([], Halt)
    )

lBind :: Spill.Bind -> InstW ()
lBind (Spill.Bind (Spill.Spill i) v) = lValue L1 v >> tell [Init S0 i L1]
lBind (Spill.Bind (Spill.AReg i) v) = lValue (AReg i) v
lBind (Spill.Bind (Spill.TReg i) v) = lValue (TReg i) v

lValue :: Reg -> Spill.Value -> InstW ()
lValue rd Spill.Unit = tell [AddImm rd Zero 0]
lValue rd (Spill.Num i) = tell [AddImm rd Zero i]
lValue rd (Spill.Proj i alloc) = do
  reg <- lRand1 alloc
  tell [Fetch rd reg i]
lValue rd (Spill.Tuple allocs) = do
  allocInsts rd (length allocs)
  traverse_ f (zip [0 ..] allocs)
  where
    f (i, alloc) = do
      rs <- lRand1 alloc
      tell [Init rd i rs]
lValue rd (Spill.Prim Add [alloc1, alloc2]) = do
  (rs1, rs2) <- lRand2 alloc1 alloc2
  tell [IAdd rd rs1 rs2]
lValue rd (Spill.Prim Sub [alloc1, alloc2]) = do
  (rs1, rs2) <- lRand2 alloc1 alloc2
  tell [ISub rd rs1 rs2]
lValue rd (Spill.Prim Mul [alloc1, alloc2]) = do
  (rs1, rs2) <- lRand2 alloc1 alloc2
  tell [IMul rd rs1 rs2]
lValue _ (Spill.Prim Print [alloc]) = do
  rs <- lRand1 alloc
  tell [CPrint rs]

lRand :: Reg -> Spill.Alloc -> InstW Reg
lRand _ (Spill.AReg i) = return (AReg i)
lRand _ (Spill.TReg i) = return (TReg i)
lRand rd (Spill.Spill i) = tell [Fetch rd S0 i] >> return rd
lRand rd (Spill.Label l) = tell [LoadLabel rd l] >> return rd

lRand1 :: Spill.Alloc -> InstW Reg
lRand1 = lRand L0

lRand2 :: Spill.Alloc -> Spill.Alloc -> InstW (Reg, Reg)
lRand2 alloc1 alloc2 = do
  rs1 <- lRand1 alloc1
  rs2 <- lRand (if needLoad alloc1 then L1 else L0) alloc2
  return (rs1, rs2)
  where
    needLoad (Spill.AReg _) = False
    needLoad (Spill.TReg _) = False
    needLoad (Spill.Spill _) = True
    needLoad (Spill.Label _) = True

allocInsts :: Reg -> Int -> InstW ()
allocInsts _ 0 = return ()
allocInsts rd size = tell [AddImm rd SP 0, AddImm SP SP (8 * size)]