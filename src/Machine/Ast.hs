module Machine.Ast where

import Text.Printf (printf)
import Utils.Ident (Ident, PrimOp (..))

data Prog = Prog Func [Func]

data Func = Func Ident Ident [Bind] Block

data Bind = Bind Ident Value | Init Ident Int Ident

data Value
  = Null
  | Num Int
  | Str String
  | Fetch Int Ident
  | AllocTuple Int
  | AllocTag Int
  | Prim PrimOp [Ident]

data Block = Block [Bind] Trans

data Trans
  = App Ident Ident
  | If0 Ident Block Block
  | Case Ident (Ident, Block) (Ident, Block)
  | Halt Ident

showlist :: (Show a) => [a] -> String
showlist list = unlines (show <$> list)

preDef :: [Func] -> [String]
preDef = map (\(Func f arg _ _) -> printf "void %s(value_t %s);" f arg)

callMain :: Func -> String
callMain (Func m _ _ _) = printf "int main() \n{\n    %s(0);\n    return 0;\n}" m

instance Show Prog where
  show (Prog m f) = printf "%s%s%s" (unlines (preDef (m : f))) (showlist (m : f)) (callMain m)

instance Show Func where
  show (Func f arg binds b) = printf "void %s(value_t %s)\n{\n%s%s}" f arg (showlist binds) (show b)

instance Show Bind where
  show (Bind x v) = printf "    value_t %s = (value_t)(%s);" x (show v)
  show (Init x i y) = printf "    ((value_t *)%s)[%d] = (value_t)(%s);" x i y

instance Show Block where
  show (Block bs trans) = printf "%s%s\n" (showlist bs) (show trans)

instance Show Value where
  show Null = "NULL"
  show (Num i) = show i
  show (Str s) = s
  show (Fetch i x) = printf "((value_t *)%s)[%d]" x i
  show (AllocTuple i) = printf "allocTuple(%d)" i
  show (AllocTag i) = printf "allocTag(%d)" i
  show (Prim Add [x, y]) = printf "%s + %s" x y
  show (Prim Sub [x, y]) = printf "%s - %s" x y
  show (Prim Mul [x, y]) = printf "%s * %s" x y
  show (Prim Print [x]) = printf "print(%s)" x
  show (Prim Int2Str [x]) = printf "toString(%s)" x

instance Show Trans where
  show (Halt x) = printf "    halt(%s);" x
  show (App f arg) = printf "    ((func_t)%s)(%s);" f arg
  show (If0 x b1 b2) = printf "if (%s == 0)\n{\n%s\n}\nelse\n{\n%s\n}" x (show b1) (show b2)