module AST where


data BinOp = Add | Minus | Times | Divide | Mod 
           deriving Show
          
data RelOp = Equal| Diff | GreaterThan | LessThan | GreaterEq | LessEq
            deriving Show

data Exp = Num Int
         | Boolean Bool
         | Var String
         | BinOp BinOp Exp Exp
         | RelOp RelOp Exp Exp
         | CallFun String [Exp]
         deriving Show

data TType = TTypeInt
          | TTypeBool
          deriving Show

data Stm = Expression Exp
         | Atr String Exp
         | InitVar TType String
         | IfStm Exp Stm
         | IfElseStm Exp Stm Stm
         | Stms Stms
         | While Exp Stm
         | Return Exp
         deriving Show

data Arg = ArgValue TType String
          deriving Show

data Fun = Func TType String [Arg] Stm
         deriving Show

type Stms = [Stm]
type FunList = [Fun]
type ArgList = [Arg]