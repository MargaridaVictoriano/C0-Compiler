module Icode where
import AST
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

data Instr = MOVE Temp Temp
           | MOVEI Temp Int 
           | OP BinOp Temp Temp Temp
           | ROP RelOp Temp Temp Temp  
           | OPI BinOp Temp Temp Int 
           | COND Temp RelOp Temp Label Label
           | CALL String [Temp]
           | FUNC Label [Argr] [Instr]
           | RETURN Temp
           | SCANINT 
           | LABEL Label 
           | JUMP Label
           deriving Show

type Temp = String
type Label = String
type Func = String
type Argr = String


type Count = (Int,Int,Int,Int)

type Table = Map String String

newTemp :: State Count Temp
newTemp = do (temps,labels,funcs,args)<-get
             put (temps +1,labels,funcs,args)
             return ("T" ++ show temps)

newLabel :: State Count Label
newLabel = do (temps,labels,funcs,args) <- get
              put (temps,labels +1,funcs,args)
              return ("L" ++ show labels)

newFun :: State Count String
newFun = do (temps,labels,f,args)<-get
            put (temps,labels,f+1,args)
            return ("F" ++ show f)


newArg :: State Count String
newArg = do (temps, labels, funcs, args)<-get
            put (temps,labels,funcs,args+1)
            return ("A" ++ show args)
            
transExpr :: Exp -> Table -> Temp -> State Count [Instr]
transExpr (Num n) table dest = return [MOVEI dest n]
-- transExpr (ScanInt ) table dest = return[SCANINT]
transExpr (Boolean True) table dest = return [MOVEI dest 1]
transExpr (Boolean False) table dest = return [MOVEI dest 0]
transExpr (Var id) table dest
  = do case Map.lookup  id table of
            Nothing -> error "Variable not declared."
            Just temp -> return [MOVE dest temp]

transExpr (BinOp op e1 e2 ) table dest
  = do t1 <- newTemp
       t2 <- newTemp
       code1 <- transExpr e1 table t1
       code2 <- transExpr e2 table t2
       return (code1 ++ code2 ++ [OP op dest t1 t2 ])

transExpr (RelOp op e1 e2) table dest
  = do l1 <- newLabel
       l2 <- newLabel
       code <- transComp (RelOp op e1 e2) table l1 l2 
       return code

transExpr (CallFun id exps) table dest
  = do case Map.lookup id table of 
            Nothing -> error "Undefined functions."
            Just labelFun -> do 
                             code <- transExprs table exps
                             return (code ++ [CALL id [dest]]) 

transExprs :: Table -> [Exp] -> State Count [Instr]
transExprs table [] = return []
transExprs table (exp : exps)
  = do temp <- newTemp
       code <- transExpr exp table temp
       code1 <- transExprs table exps
       return (code ++ code1)

transStm :: Stm -> Table -> State Count [Instr]
transStm (InitVar ttype id) table = return []
transStm (Atr id expr) table
  = do case Map.lookup id table of
            Nothing -> error "Variable not declared."
            Just dest -> do
                 code <- transExpr expr table dest
                 return code

transStm (Return exp) table = do
  t <- newTemp
  code <- transExpr exp table t 
  return (code ++ [RETURN t])

transStm ( Expression exp) table = do
  t <- newTemp
  code <- transExpr exp table t 
  return code


transStm (IfStm exp stm) table = do
  label1 <- newLabel
  label2 <- newLabel
  code1 <- transComp exp table label1 label2
  code2 <- transStm stm table
  return (code1 ++ [LABEL label1] ++ code2 ++ [LABEL label2])


transStm (IfElseStm exp stm1 stm2) table = do
  label1 <- newLabel
  label2 <- newLabel
  label3 <- newLabel
  code1  <- transComp exp table label1 label2
  code2 <- transStm stm1 table
  code3 <- transStm stm2 table
  return (code1 ++ [LABEL label1] ++ code2 ++ [JUMP label3] ++ [LABEL label2])

transStm (While exp stm) table = do
  label1 <- newLabel
  label2 <- newLabel
  label3 <- newLabel
  code1 <- transComp exp table label2 label3
  code2 <- transStm stm table
  return ([LABEL label1] ++ code1 ++ [LABEL label2] ++ code2 ++ [JUMP label1])

transStm (Stms l) table = do 
  code <- transStms l table
  return code 

transStms :: [Stm] -> Table -> State Count [Instr]
transStms [] _ = return []
transStms (stm:stms) table = do
  code1 <- transStm stm table
  code2 <- transStms stms table
  return (code1 ++ code2)

transComp :: Exp -> Table -> Label -> Label -> State Count [Instr]
transComp (RelOp op e1 e2) table labelT labelF
  = do t1 <- newTemp
       t2 <- newTemp
       code1 <- transExpr e1 table t1
       code2 <- transExpr e2 table t2 
       return (code1 ++ code2 ++ [COND t1 op t2 labelT labelF])
       
transComp expr table labelT labelF 
  = do t1 <- newTemp
       t2 <- newTemp
       code1 <- transExpr expr table t1
       code2 <- transExpr (Num 0) table t2
       return (code1 ++ [COND t1 Diff t2 labelT labelF])

intermediate :: FunList -> Table -> State Count [Instr]
intermediate [] table = return []
intermediate ((Func t id arglist stm) : funcs ) table
  = do table <- if(Map.null table)
            then (getFuns ((Func t id arglist stm):funcs)) table
            else return table

       tab    <- getVals [stm] table
       temp   <- newTemp
       result <- transFuns (Func t id arglist stm ) tab temp
       re     <- intermediate funcs table  
       return (result : re)

getVals :: Stms -> Table -> State Count Table
getVals [] table = return table
getVals (stm:stms) table = case stm of
  (InitVar tbl id) -> do
    temp <- newTemp;
    return (Map.insert id temp table)
  _ -> getVals stms table 


getFuns :: FunList -> Table -> State Count Table
getFuns [] table = return table
getFuns (fun : funs) table
  = do aux <- insertFun fun table
       rt  <- getFuns funs aux
       return rt  

insertFun :: Fun -> Table -> State Count Table
insertFun (Func tbl id args stm) table  = do 
  fun <- newFun
  return (Map.insert id fun table)


transFuns :: Fun -> Table -> Temp -> State Count Instr
transFuns (Func t id arglist stm ) table dest = do
  (args,tabl) <- argTemp arglist table
  stms <- transStm stm table 
  return (FUNC id args stms)

argTemp :: ArgList -> Table -> State Count ([Temp], Table)
argTemp [] table = return ([], table)
argTemp ((ArgValue tp id):args) table = do
  (a, tb) <- insertArg id tp table 
  (b, tbl) <- argTemp args tb
  return ((a:b),tbl)

insertArg :: String -> TType -> Table -> State Count (Argr, Table)
insertArg id _ table = do 
  arg <- newArg
  return (arg, Map.insert id arg table)

