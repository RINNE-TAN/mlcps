module Machine.Pretty where

import Machine.Ast (Block (..), Inst (..), LastInst (..), Prog (..), Reg (..))
import Text.PrettyPrint (Doc, ($+$), (<+>))
import qualified Text.PrettyPrint as PP

class Disp t where
  display :: t -> Doc

instance (Disp t) => (Disp [t]) where
  display list = PP.vcat (display <$> list)

tab :: Doc -> Doc
tab = PP.nest 4

(!) :: Reg -> Int -> Doc
(!) reg idx = PP.int (8 * idx) <> PP.parens (display reg)

instance Disp Reg where
  display (AReg i) = "a" <> PP.int i
  display (TReg i) = "t" <> PP.int (i + 2)
  display L0 = "t0"
  display L1 = "t1"
  display S0 = "s0"
  display SP = "sp"
  display Zero = "zero"

instance Disp Inst where
  display (IAdd rd rs1 rs2) = "add" <+> display rd <+> "," <+> display rs1 <+> "," <+> display rs2
  display (ISub rd rs1 rs2) = "sub" <+> display rd <+> "," <+> display rs1 <+> "," <+> display rs2
  display (IMul rd rs1 rs2) = "mul" <+> display rd <+> "," <+> display rs1 <+> "," <+> display rs2
  display (Fetch rd rs imm) = "ld" <+> display rd <+> "," <+> (rs ! imm)
  display (Init rd imm rs) = "sd" <+> display rs <+> "," <+> (rd ! imm)
  display (LoadLabel rd label) = "la" <+> display rd <+> "," <+> PP.text label
  display (AddImm rd rs imm) = "addi" <+> display rd <+> "," <+> display rs <+> "," <+> PP.int imm
  display (Move rd rs) = "mv" <+> display rd <+> "," <+> display rs
  display (CPrint _) = undefined

instance Disp LastInst where
  display (Jump x) = "jr" <+> display x
  display (If0 x b1 b2) =
    "beq" <+> display x <+> "," <+> "zero" <+> "," <+> PP.text b1
      $+$ "j" <+> PP.text b2
  display Halt = "j" <+> "halt"

instance Disp Block where
  display (Block b insts lastInst) =
    PP.text b <> ":"
      $+$ tab (display insts)
      $+$ tab (display lastInst)

instance Disp Prog where
  display (Prog m@(Block start _ _) f) =
    ".global main"
      $+$ "main:"
      $+$ tab "la s0 , _stack_bottom"
      $+$ tab "la sp , _stack_bottom"
      $+$ tab "j" <+> PP.text start
      $+$ display (m : f)
      $+$ ""
