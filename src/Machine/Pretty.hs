module Machine.Pretty where

import Machine.Ast (Bind (..), Block (..), Func (..), Prog (..), Trans (..), Value (..))
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
  display Null = "NULL"
  display (Num i) = PP.int i
  display (Str s) = PP.text s
  display (Fetch i x) = PP.text x ! i
  display (AllocTuple n) = "allocTuple" @ PP.int n
  display (AllocTag n) = "allocTag" @ PP.int n
  display (Prim Add [x, y]) = PP.text x <+> "+" <+> PP.text y
  display (Prim Sub [x, y]) = PP.text x <+> "-" <+> PP.text y
  display (Prim Mul [x, y]) = PP.text x <+> "*" <+> PP.text y
  display (Prim Print [x]) = "print" @ PP.text x
  display (Prim Int2Str [x]) = "toString" @ PP.text x

instance Disp Bind where
  display (Bind x v) =
    ( "value_t"
        <+> PP.text x
        <+> "="
        <+> (PP.parens "value_t" <> display v)
    )
      <> ";"
  display (Init x i y) = (PP.text x ! i <+> "=" <+> (PP.parens "value_t" <> PP.text y)) <> ";"

instance Disp Trans where
  display (Halt x) =
    "GLOBAL_FUNC = (value_t)halt;"
      $+$ (PP.text "GLOBAL_ARG" <+> "=" <+> PP.text x) <> ";"
  display (App f arg) =
    (PP.text "GLOBAL_FUNC" <+> "=" <+> PP.text f) <> ";"
      $+$ (PP.text "GLOBAL_ARG" <+> "=" <+> PP.text arg) <> ";"
  display (If0 x b1 b2) =
    "if" <+> PP.parens (PP.text x <+> "==" <+> PP.int 0)
      $+$ "{"
      $+$ tab (display b1)
      $+$ "}"
      $+$ "else"
      $+$ "{"
      $+$ tab (display b2)
      $+$ "}"

instance Disp Block where
  display (Block bs trans) =
    display bs
      $+$ display trans

instance Disp Func where
  display (Func f arg binds b) =
    "void" <+> (PP.text f <> PP.parens ("value_t" <+> PP.text arg))
      $+$ "{"
      $+$ tab (display binds)
      $+$ tab (display b)
      $+$ "}"

instance Disp Prog where
  display (Prog m f) =
    PP.text "#include <stdio.h>"
      $+$ PP.text "#include \"runtime.h\""
      $+$ PP.vcat
        ( ( \(Func fname arg _ _) ->
              "void" <+> (PP.text fname <> PP.parens ("value_t" <+> PP.text arg) <> ";")
          )
            <$> (m : f)
        )
      $+$ display (m : f)
      $+$ PP.text "int main()"
      $+$ PP.text "{"
      $+$ tab (("GLOBAL_FUNC" <+> "=" <+> PP.parens "value_t") <> PP.text (name m) <> ";")
      $+$ tab "main_loop();"
      $+$ tab "return 0;"
      $+$ PP.text "}"
    where
      name (Func fname _ _ _) = fname