module Pascal.Interpret 
(
    interpret
)
where

import Pascal.Data
import Pascal.Scope
import Pascal.Function

import qualified Data.Map as Map

-- Evaluate boolean expression
evalBoolExp :: BoolExp -> Scope -> FuncTable -> ValueT
evalBoolExp True_C _ _ = Bool True 
evalBoolExp False_C _ _ = Bool False 

-- and
evalBoolExp (OpB "and" e1 e2) scope table = 
  Bool (and [toBool $ evalBoolExp e1 scope table, toBool $ evalBoolExp e2 scope table])
-- or
evalBoolExp (OpB "or" e1 e2) scope table = 
  Bool (or [toBool $ evalBoolExp e1 scope table, toBool $ evalBoolExp e2 scope table])
-- xor
evalBoolExp (OpB "xor" e1 e2) scope table = 
  Bool (toBool (evalBoolExp e1 scope table) /= toBool (evalBoolExp e2 scope table))
-- not
evalBoolExp (Not e) scope table = 
  Bool (not $ toBool $ evalBoolExp e scope table)

-- Evaluate comparison
evalBoolExp (Comp ">" e1 e2) scope table = 
  Bool (toFloat (evalRealExp e1 scope table) > toFloat (evalRealExp e2 scope table))

evalBoolExp (Comp "<" e1 e2) scope table = 
  Bool (toFloat (evalRealExp e1 scope table) < toFloat (evalRealExp e2 scope table))
  
evalBoolExp (Comp "=" e1 e2) scope table = 
  Bool (toFloat (evalRealExp e1 scope table) == toFloat (evalRealExp e2 scope table))

evalBoolExp (Comp ">=" e1 e2) scope table = 
  Bool (toFloat (evalRealExp e1 scope table) >= toFloat (evalRealExp e2 scope table))

evalBoolExp (Comp "<=" e1 e2) scope table = 
  Bool (toFloat (evalRealExp e1 scope table) <= toFloat (evalRealExp e2 scope table))

evalBoolExp (Comp "<>" e1 e2) scope table = 
  Bool (toFloat (evalRealExp e1 scope table) /= toFloat (evalRealExp e2 scope table))

-- Variable
evalBoolExp (VarBool name) scope _ = getVal name scope

-- Function
evalBoolExp (FuncBool name params) scope table = getVal name newScope
  where (str, newScope) = executeFunc name params scope table


-- Evaluate real expressions
evalRealExp :: RealExp -> Scope -> FuncTable -> ValueT
evalRealExp (Real n) _ _ = Float n 
evalRealExp (Integer n) _ _ = Float $ fromIntegral n 

evalRealExp (Op1 "-" e) scope table =  Float $ - toFloat (evalRealExp e scope table)

-- Addition
evalRealExp (Op2 "+" e1 e2) scope table = 
  Float (toFloat (evalRealExp e1 scope table) + toFloat (evalRealExp e2 scope table))
-- Subtraction
evalRealExp (Op2 "-" e1 e2) scope table = 
  Float (toFloat (evalRealExp e1 scope table) - toFloat (evalRealExp e2 scope table))
-- Multiplcation
evalRealExp (Op2 "*" e1 e2) scope table = 
  Float (toFloat (evalRealExp e1 scope table) * toFloat (evalRealExp e2 scope table))
-- Division
evalRealExp (Op2 "/" e1 e2) scope table = 
  Float (toFloat (evalRealExp e1 scope table) / toFloat (evalRealExp e2 scope table))

-- square root
evalRealExp (Sqrt e) scope table = Float (sqrt $ toFloat $ evalRealExp e scope table)
-- sin
evalRealExp (Sin e) scope table = Float (sin $ toFloat $ evalRealExp e scope table)
-- cos
evalRealExp (Cos e) scope table = Float (cos $ toFloat $ evalRealExp e scope table)
-- natural log
evalRealExp (Ln e) scope table = Float (log $ toFloat $ evalRealExp e scope table)
-- exp
evalRealExp (Exp e) scope table = Float (exp $ toFloat $ evalRealExp e scope table)

-- Variable
evalRealExp (VarReal name) scope _ = getVal name scope

-- Function
evalRealExp (FuncReal name params) scope table = getVal name newScope
  where (str, newScope) = executeFunc name params scope table


-- Evaluate general expressions
evalExp :: GenExp -> Scope -> FuncTable -> ValueT
evalExp (FloatExp exp) scope table = evalRealExp exp scope table
evalExp (BExp exp) scope table = evalBoolExp exp scope table

printList :: [Param] -> Scope -> FuncTable -> String
printList [exp] scope table = case exp of 
  RealP e -> (toString $ evalExp (FloatExp e) scope table) ++ " "
  BoolP e -> (toString $ evalExp (BExp e) scope table) ++ " "
  StrP e -> (toString $ evalExp (FloatExp (VarReal e)) scope table) ++ " "
printList (exp : tl) scope table = 
  printList [exp] scope table ++ printList tl scope table 

-- Evaluate statement
parseStmt :: Statement -> Scope -> FuncTable -> (String, Scope)
-- print statement
parseStmt (Print exp) scope table = case evalExp exp scope table of
    Float f -> (show f ++ "\n", scope)
    Bool b -> (show b ++ "\n", scope)
-- print statement with more than 1 argument
parseStmt (PrintList list) scope table = 
  ((printList list scope table) ++ "\n", scope)
-- print new line
parseStmt PrintNewLine scope table = ("\n", scope)
-- assignment
parseStmt (Assign name exp) scope table = ("", assignVal name (evalExp exp scope table) scope)
-- if else statement
parseStmt (IfElse boolExp s1 s2) scope table = case value of
  Bool True -> parseStmt s1 scope table
  Bool False -> parseStmt s2 scope table
  where value = evalExp (BExp boolExp) scope table
-- if statement
parseStmt (If boolExp stmt) scope table = case value of
  Bool True -> parseStmt stmt scope table
  Bool False -> ("", scope)
  where value = evalExp (BExp boolExp) scope table
-- case statements
parseStmt (Case var caseStmts stmts) scope table = case b of
  True -> result
  False -> processBody stmts scope table
  where (result, b) = executeCase var caseStmts scope table False

parseStmt (Block stmts) scope table = processBody stmts scope table

parseStmt (While boolExp stmt) scope table = executeWhile boolExp stmt scope "" table

parseStmt (ForUp name exp1 exp2 stmt) scope table = 
  let newTable = assignVal name (evalExp (FloatExp exp1) scope table) scope in
    executeFor "+" "<=" name exp2 stmt newTable "" table

parseStmt (ForDown name exp1 exp2 stmt) scope table = 
  let newTable = assignVal name (evalExp (FloatExp exp1) scope table) scope in
    executeFor "-" ">=" name exp2 stmt newTable "" table

parseStmt (FuncCall name vars) scope table = executeFunc name vars scope table 

parseStmt _ scope _ = ("There is a problem", scope)


-- Execute for loop
executeFor :: String -> String -> String -> RealExp -> Statement -> Scope -> String -> FuncTable -> (String, Scope)
executeFor op1 op2 var exp stmt scope str table = case value of
  Bool True -> let (newStr, newScope1) = parseStmt stmt scope table in 
               case equal of 
                    Bool True -> (str ++ newStr, newScope1)
                    Bool False -> let newScope2 = assignVal var (evalExp (FloatExp (Op2 op1 (Real varValue) (Integer 1))) newScope1 table) newScope1
                      in executeFor op1 op2 var exp stmt newScope2 (str ++ newStr) table
  Bool False -> (str, scope)
  where value = evalExp (BExp (Comp op2 (VarReal var) exp)) scope table
        equal = evalExp (BExp (Comp "=" (VarReal var) exp)) scope table
        varValue = toFloat $ evalExp (FloatExp (VarReal var)) scope table

-- Execute case statements 
executeCase :: String -> [CaseStmt] -> Scope -> FuncTable -> Bool -> ((String, Scope), Bool)
executeCase var [(Check exp stmt)] scope table b
  | target == cur = (parseStmt stmt scope table, True)
  | otherwise = (("", scope), or [b, False])
  where target = getVal var scope
        cur = evalExp exp scope table
executeCase var (stmt : tl) scope table b = ((str1 ++ str2, newScope2), b2)
  where ((str1, newScope1), b1) = executeCase var [stmt] scope table b 
        ((str2, newScope2), b2) = executeCase var tl newScope1 table (or [b, b1]) 

-- Execute while loop
executeWhile :: BoolExp -> Statement -> Scope -> String -> FuncTable -> (String, Scope)
executeWhile boolExp stmt scope str table = case value of 
  Bool True -> let (newStr, newTable) = processBody [stmt] scope table in
               executeWhile boolExp stmt newTable (str ++ newStr) table
  Bool False -> (str, scope)
  where value = evalExp (BExp boolExp) scope table

-- Execute functions or procedures
executeFunc :: String -> [Param] -> Scope -> FuncTable -> (String, Scope)
executeFunc name params scope table = (str, newScope5)
  where (vars, varRefs, defs, stmts) = getFunc name table
        newScope1 = addScope vars params scope table
        newScope2 = addRefs varRefs newScope1 
        (newScope3, newTable) = processDefs defs newScope2 table
        (str, newScope4) = processBody stmts newScope3 table
        refs = refsToStrs varRefs []
        varNames = case refs of
          [] -> [StrP name]
          otherwise -> lastN (length refs) params
        (cur : newScope5) = endScope varNames refs newScope4

-- Add new scope
addScope :: [VarDef] -> [Param] -> Scope -> FuncTable -> Scope
addScope [] [] scope _ = (emptyScope : scope)
addScope vars params scope table =
  addParams (refsToStrs vars []) params (emptyScope : scope) table

-- Add referenced var to new scope
addRefs :: [VarDef] -> Scope -> Scope
addRefs [] scope = scope
addRefs [VarDef names varType] scope = storeDef names varType scope
addRefs (ref : tl) scope = addRefs tl newScope
  where newScope = addRefs [ref] scope

-- Add parameters to new scope
addParams :: [String] -> [Param] -> Scope -> FuncTable -> Scope
addParams [name] [StrP param] scope _ = assignVal name value scope
  where value = getVal param scope 
addParams [name] [RealP param] scope table = assignVal name value scope
  where value = evalRealExp param scope table 
addParams [name] [BoolP param] scope table = assignVal name value scope
  where value = evalBoolExp param scope table
addParams (name : names) (param : params) scope table = addParams names params newScope table
  where newScope = addParams [name] [param] scope table
addParams [] params scope _ = scope
addParams _ _ scope _ = scope

-- Process statements
processBody :: Body -> Scope -> FuncTable -> (String, Scope)
processBody [stmt] scope table = parseStmt stmt scope table
processBody (stmt : tl) scope table = (str1 ++ str2, newTable2)
  where (str1, newTable1) = processBody [stmt] scope table
        (str2, newTable2) = processBody tl newTable1 table

processDefs :: Defs -> Scope -> FuncTable -> (Scope, FuncTable)
processDefs [] scope table = (scope, table)
-- Process variable definitions
processDefs [Def (VarDef vars varType)] scope table = (newScope, funcTable)
  where newScope = storeDef vars varType scope
-- Process function definitions
processDefs [Func name vars defs stmts] scope table = (scope, newTable)
  where newTable = addFunc name vars [] defs stmts table
-- Process procedure definitions
processDefs [Proc name vars varRefs defs stmts] scope table = (scope, newTable)
  where newTable = addFunc name vars varRefs defs stmts table 

processDefs (def : tl) scope funcTable = processDefs tl newScope newTable
  where (newScope, newTable) = processDefs [def] scope funcTable

getString :: (String, Scope) -> String
getString (s, table) = s

-- Evaluate program statements
interpret :: Program -> String
interpret (Process defs body) = getString $ processBody body scope table
  where (scope, table) = processDefs defs [emptyScope] funcTable
