module Pascal.Interpret 
(
    interpret
)
where

import Pascal.Data
import Pascal.EvalReal
import Pascal.EvalBool
import Pascal.Scope

import qualified Data.Map as Map

-- TODO: define auxiliary functions to aid interpretation
-- Feel free to put them here or in different modules
-- Hint: write separate evaluators for numeric and
-- boolean expressions and for statements

-- make sure you write test unit cases for all functions

-- Evaluate general expressions
evalExp :: GenExp -> Table -> ValueT
evalExp (FloatExp exp) table = evalRealExp exp table
evalExp (BExp exp) table = evalBoolExp exp table

-- Evaluate statement
parseStmt :: Statement -> Table -> (String, Table)
-- print statement
parseStmt (Print exp) table = case evalExp exp table of
    Float f -> (show f ++ "\n", table)
    Bool b -> (show b ++ "\n", table)
-- assignment
parseStmt (Assign name exp) table = ("", assignVal name (evalExp exp table) table)
-- if else statement
parseStmt (If boolExp block1 block2) table = case value of
  Bool True -> case block1 of 
    Stmt s -> parseStmt s table
    Body b -> processBody b table
  Bool False -> case block2 of
    Stmt s -> parseStmt s table
    Body b -> processBody b table
  where value = evalExp (BExp boolExp) table
-- case statements
parseStmt (Case var caseStmts stmts) table = case b of
  True -> result
  False -> processBody stmts table
  where (result, b) = executeCase var caseStmts table False

parseStmt (Block stmts) table = processBody stmts table

parseStmt (While boolExp stmt) table = executeWhile boolExp stmt table ""

parseStmt (ForUp name exp1 exp2 stmt) table = 
  let newTable = assignVal name (evalExp (FloatExp exp1) table) table in
    executeFor "+" "<=" name exp2 stmt newTable ""

parseStmt (ForDown name exp1 exp2 stmt) table = 
  let newTable = assignVal name (evalExp (FloatExp exp1) table) table in
    executeFor "-" ">=" name exp2 stmt newTable ""

parseStmt _ table = ("There is a problem", table)

-- Execute for loop
executeFor :: String -> String -> String -> RealExp -> Statement -> Table -> String -> (String, Table)
executeFor op1 op2 var exp stmt table str = case value of
  Bool True -> let (newStr, newTable1) = parseStmt stmt table in 
                let newTable2 = assignVal var (evalExp (FloatExp (Op2 op1 (Real varValue) (Integer 1))) newTable1) newTable1
                    in executeFor op1 op2 var exp stmt newTable2 (str ++ newStr)
  Bool False -> (str, table)
  where value = evalExp (BExp (Comp op2 (VarReal var) exp)) table
        varValue = toFloat $ evalExp (FloatExp (VarReal var)) table

-- Execute case statements 
executeCase :: String -> [CaseStmt] -> Table -> Bool -> ((String, Table), Bool)
executeCase var [(Check exp stmt)] table b
  | target == cur = (parseStmt stmt table, True)
  | otherwise = (("", table), or [b, False])
  where target = getVal var table
        cur = evalExp exp table 
executeCase var (stmt : tl) table b = ((str1 ++ str2, newTable2), b2)
  where ((str1, newTable1), b1) = executeCase var [stmt] table b
        ((str2, newTable2), b2) = executeCase var tl newTable1 (or [b, b1])

-- Execute while loop
executeWhile :: BoolExp -> Statement -> Table -> String -> (String, Table)
executeWhile boolExp stmt table str = case value of 
  Bool True -> let (newStr, newTable) = processBody [stmt] table in
               executeWhile boolExp stmt newTable (str ++ newStr)
  Bool False -> (str, table)
  where value = evalExp (BExp boolExp) table 

-- Process statements
processBody :: Body -> Table -> (String, Table)
processBody [stmt] table = parseStmt stmt table
processBody (stmt : tl) table = (str1 ++ str2, newTable2)
  where (str1, newTable1) = processBody [stmt] table
        (str2, newTable2) = processBody tl newTable1

-- Process definitions
processDefs :: Defs -> [(String, ValueT)] -> [(String, ValueT)]
processDefs [def] list = (parseDef def []) ++ list
processDefs (def : tl) list = (processDefs [def] list) ++ (processDefs tl list)

getString :: (String, Table) -> String
getString (s, table) = s

-- Evaluate program statements
interpret :: Program -> String
interpret (Process defs body) = getString $ processBody body (buildTable $ (processDefs defs []))

interpret _ = "Not implemented"