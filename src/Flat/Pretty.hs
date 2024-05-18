module Flat.Pretty where

import Flat.Ast (Bind (..), Func (..), Prog (..), Trans (..), Value (..))
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

instance Disp Value where
  display Unit = PP.text "(value_t)NULL"
  display (Num i) = PP.int i
  display (Str s) = PP.text s
  display (Proj i x) = PP.text x ! i
  display (Prim Add [x, y]) = PP.text x <+> "+" <+> PP.text y
  display (Prim Sub [x, y]) = PP.text x <+> "-" <+> PP.text y
  display (Prim Mul [x, y]) = PP.text x <+> "*" <+> PP.text y
  display (Prim Print [x]) = "print" @ PP.text x
  display (Prim Int2Str [x]) = "toString" @ PP.text x

instance Disp Bind where
  display (Bind x (Tuple vs)) =
    ("value_t" <+> PP.text x <+> "=" <+> ("allocTuple" @ PP.int (length vs))) <> ";"
      $+$ PP.vcat (f <$> zip [0 ..] vs)
    where
      f (i, y) = (PP.text x ! i <+> "=" <+> (PP.parens "value_t" <> PP.text y)) <> ";"
  display (Bind x v) = ("value_t" <+> PP.text x <+> "=" <+> display v) <> ";"

instance Disp Trans where
  display (Halt x) =
    "GLOBAL_FUNC = (value_t)halt;"
      $+$ ("GLOBAL_ARG" <+> "=" <+> PP.text x) <> ";"
  display (App f arg) =
    ("GLOBAL_FUNC" <+> "=" <+> PP.text f) <> ";"
      $+$ ("GLOBAL_ARG" <+> "=" <+> PP.text arg) <> ";"
  display (If0 x b1 b2) =
    "if" <+> PP.parens (PP.text x <+> "==" <+> PP.int 0)
      $+$ "{"
      $+$ tab (display b1)
      $+$ "}"
      $+$ "else"
      $+$ "{"
      $+$ tab (display b2)
      $+$ "}"

instance Disp Func where
  display (Func f arg binds b) =
    "void" <+> (PP.text f <> PP.parens ("value_t" <+> PP.text arg))
      $+$ "{"
      $+$ tab (display binds)
      $+$ tab (display b)
      $+$ "}"

instance Disp Prog where
  display (Prog m f) =
    "#include <stdio.h>"
      $+$ "#include \"runtime.h\""
      $+$ PP.vcat
        ( ( \(Func fname arg _ _) ->
              "void" <+> (PP.text fname <> PP.parens ("value_t" <+> PP.text arg) <> ";")
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
