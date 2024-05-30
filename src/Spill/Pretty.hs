module Spill.Pretty where

import Spill.Ast (Alloc (..), Bind (..), Func (..), Prog (..), Trans (..), Value (..))
import Text.PrettyPrint (Doc, ($+$), (<+>))
import qualified Text.PrettyPrint as PP
import Utils.Ident (PrimOp (..))

class Disp t where
  display :: t -> Doc

instance (Disp t) => (Disp [t]) where
  display list = PP.vcat (display <$> list)

(!) :: Doc -> Int -> Doc
(!) d idx = PP.parens (PP.parens "value_t *" <> d) <> PP.brackets (PP.int idx)

(@) :: Doc -> Doc -> Doc
(@) f arg = f <> PP.parens arg

tab :: Doc -> Doc
tab = PP.nest 4

instance Disp Alloc where
  display (AReg i) = "REG_A" <> PP.int i
  display (TReg i) = "REG_T" <> PP.int i
  display (Label a) = PP.parens "value_t" <> PP.text a
  display (Spill i) = "REG_BP" ! i

instance Disp Value where
  display Unit = PP.text "(value_t)NULL"
  display (Num i) = PP.int i
  display (Str s) = PP.text s
  display (Proj i x) = display x ! i
  display (Prim Add [x, y]) = display x <+> "+" <+> display y
  display (Prim Sub [x, y]) = display x <+> "-" <+> display y
  display (Prim Mul [x, y]) = display x <+> "*" <+> display y
  display (Prim Print [x]) = "print" @ display x
  display (Prim Int2Str [x]) = "toString" @ display x

instance Disp Bind where
  display (Bind x (Tuple vs)) =
    (display x <+> "=" <+> "REG_SP") <> ";"
      $+$ PP.vcat (finit <$> zip [0 ..] vs)
      $+$ ("REG_SP" <+> "+=" <+> "sizeof(value_t)" <+> "*" <+> PP.int (length vs)) <> ";"
    where
      finit (i, y) = ("REG_SP" ! i <+> "=" <+> display y) <> ";"
  display (Bind x v) = (display x <+> "=" <+> display v) <> ";"

instance Disp Trans where
  display (Halt x) =
    "GLOBAL_FUNC = (value_t)halt;"
      $+$ ("REG_A0" <+> "=" <+> display x) <> ";"
  display (App f args) =
    ("GLOBAL_FUNC" <+> "=" <+> display f) <> ";"
      $+$ PP.vcat (finit <$> zip [0 ..] args)
    where
      finit (i, y) = (("REG_A" <> PP.int i) <+> "=" <+> display y) <> ";"
  display (If0 x b1 b2) =
    "if" <+> PP.parens (display x <+> "==" <+> PP.int 0)
      $+$ "{"
      $+$ tab (display b1)
      $+$ "}"
      $+$ "else"
      $+$ "{"
      $+$ tab (display b2)
      $+$ "}"

instance Disp Func where
  display (Func f nAlloc binds b) =
    "void" <+> (PP.text f <> PP.parens "")
      $+$ "{"
      $+$ tab ("REG_BP" <+> "=" <+> "REG_SP") <> ";"
      $+$ tab ("REG_SP" <+> "+=" <+> "sizeof(value_t)" <+> "*" <+> PP.int nAlloc) <> ";"
      $+$ tab (display binds)
      $+$ tab (display b)
      $+$ "}"

instance Disp Prog where
  display (Prog m f) =
    "#include <stdio.h>"
      $+$ "#include \"runtime.h\""
      $+$ PP.vcat
        ( ( \(Func fname _ _ _) ->
              "void" <+> (PP.text fname <> PP.parens "" <> ";")
          )
            <$> (m : f)
        )
      $+$ display (m : f)
      $+$ "int main()"
      $+$ "{"
      $+$ tab (("GLOBAL_FUNC" <+> "=" <+> PP.parens "value_t") <> PP.text (name m) <> ";")
      $+$ tab "main_loop();"
      $+$ tab "return 0;"
      $+$ "}"
    where
      name (Func fname _ _ _) = fname
