module Machine.Pretty where

import Machine.Ast (Block (..), Inst (..), LastInst (..), Prog (..), Reg (..))
import Text.PrettyPrint (Doc, ($+$), (<+>))
import qualified Text.PrettyPrint as PP

class Disp t where
  display :: t -> Doc

instance (Disp t) => (Disp [t]) where
  display list = PP.vcat (display <$> list)

line :: Doc -> Doc
line doc = doc <> ";"

tab :: Doc -> Doc
tab = PP.nest 4

(!) :: Reg -> Int -> Doc
(!) reg idx = PP.parens (PP.parens "value_t *" <> display reg) <> PP.brackets (PP.int idx)

toValue :: Doc -> Doc
toValue d = PP.parens "value_t" <> d

instance Disp Reg where
  display (AReg i) = "REG_A" <> PP.int i
  display (TReg i) = "REG_T" <> PP.int i
  display L0 = "REG_L0"
  display L1 = "REG_L1"
  display S0 = "REG_S0"
  display SP = "REG_SP"
  display Zero = "REG_ZERO"

instance Disp Inst where
  display (IAdd rd rs1 rs2) = line $ display rd <+> "=" <+> display rs1 <+> "+" <+> display rs2
  display (ISub rd rs1 rs2) = line $ display rd <+> "=" <+> display rs1 <+> "-" <+> display rs2
  display (IMul rd rs1 rs2) = line $ display rd <+> "=" <+> display rs1 <+> "*" <+> display rs2
  display (Fetch rd rs imm) = line $ display rd <+> "=" <+> (rs ! imm)
  display (Init rd imm rs) = line $ (rd ! imm) <+> "=" <+> display rs
  display (LoadLabel rd label) = line $ display rd <+> "=" <+> toValue (PP.text label)
  display (AddImm rd rs imm) = line $ display rd <+> "=" <+> display rs <+> "+" <+> PP.int imm
  display (CPrint x) = line $ "print" <> PP.parens (display x)

instance Disp LastInst where
  display (Jump x) = line $ "GLOBAL_FUNC" <+> "=" <+> display x
  display (If0 x b1 b2) =
    "if" <+> PP.parens (display x <+> "==" <+> PP.int 0)
      $+$ "{"
      $+$ tab (line $ "GLOBAL_FUNC" <+> "=" <+> toValue (PP.text b1))
      $+$ "}"
      $+$ "else"
      $+$ "{"
      $+$ tab (line $ "GLOBAL_FUNC" <+> "=" <+> toValue (PP.text b2))
      $+$ "}"
  display Halt = line $ "GLOBAL_FUNC" <+> "=" <+> toValue "halt"

instance Disp Block where
  display (Block b insts lastInst) =
    "void" <+> (PP.text b <> PP.parens "")
      $+$ "{"
      $+$ tab (display insts)
      $+$ tab (display lastInst)
      $+$ "}"

instance Disp Prog where
  display (Prog m f) =
    "#include <stdio.h>"
      $+$ "#include \"runtime.h\""
      $+$ PP.vcat
        ( ( \b ->
              line $ "void" <+> (PP.text (name b) <> PP.parens "")
          )
            <$> (m : f)
        )
      $+$ display (m : f)
      $+$ "int main()"
      $+$ "{"
      $+$ tab (line $ "GLOBAL_FUNC" <+> "=" <+> toValue (PP.text (name m)))
      $+$ tab "main_loop();"
      $+$ tab "return 0;"
      $+$ "}"
    where
      name (Block fname _ _) = fname