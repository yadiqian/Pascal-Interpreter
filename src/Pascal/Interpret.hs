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
evalBoolExp :: BoolExp -> Scope -> FuncTable -> String -> (ValueT, String)
evalBoolExp True_C _ _ str = (Bool True , str)
evalBoolExp False_C _ _ str = (Bool False, str) 

-- and
evalBoolExp (OpB "and" e1 e2) scope table str = (Bool (and [toBool val1, toBool val2]), str1 ++ str2) 
  where (val1, str1) = evalBoolExp e1 scope table str
        (val2, str2) = evalBoolExp e2 scope table str
-- or
evalBoolExp (OpB "or" e1 e2) scope table str = (Bool (or [toBool val1, toBool val2]), str1 ++ str2) 
  where (val1, str1) = evalBoolExp e1 scope table str
        (val2, str2) = evalBoolExp e2 scope table str
-- xor
evalBoolExp (OpB "xor" e1 e2) scope table str = (Bool (val1 /= val2), str1 ++ str2) 
  where (val1, str1) = evalBoolExp e1 scope table str
        (val2, str2) = evalBoolExp e2 scope table str
-- not
evalBoolExp (Not e) scope table str = (Bool (not $ toBool val), newStr)
  where (val, newStr) = evalBoolExp e scope table str

-- Evaluate comparison
evalBoolExp (Comp ">" e1 e2) scope table str = (Bool (toFloat val1 > toFloat val2), str1 ++ str2)
  where (val1, str1) = evalRealExp e1 scope table str
        (val2, str2) = evalRealExp e2 scope table str

evalBoolExp (Comp "<" e1 e2) scope table str = (Bool (toFloat val1 < toFloat val2), str1 ++ str2)
  where (val1, str1) = evalRealExp e1 scope table str
        (val2, str2) = evalRealExp e2 scope table str
  
evalBoolExp (Comp "=" e1 e2) scope table str = (Bool (toFloat val1 == toFloat val2), str1 ++ str2)
  where (val1, str1) = evalRealExp e1 scope table str
        (val2, str2) = evalRealExp e2 scope table str

evalBoolExp (Comp ">=" e1 e2) scope table str = (Bool (toFloat val1 >= toFloat val2), str1 ++ str2)
  where (val1, str1) = evalRealExp e1 scope table str
        (val2, str2) = evalRealExp e2 scope table str

evalBoolExp (Comp "<=" e1 e2) scope table str = (Bool (toFloat val1 <= toFloat val2), str1 ++ str2)
  where (val1, str1) = evalRealExp e1 scope table str
        (val2, str2) = evalRealExp e2 scope table str

evalBoolExp (Comp "<>" e1 e2) scope table str = (Bool (val1 /= val2), str1 ++ str2)
  where (val1, str1) = evalRealExp e1 scope table str
        (val2, str2) = evalRealExp e2 scope table str

-- Variable
evalBoolExp (VarBool name) scope _ str = (getVal name scope, str)

-- Function
evalBoolExp (FuncBool name params) scope table str = (getVal name newScope, newStr)
  where (newStr, newScope) = executeFunc name params scope table


-- Evaluate real expressions
evalRealExp :: RealExp -> Scope -> FuncTable -> String -> (ValueT, String)
evalRealExp (Real n) _ _ str = (Float n, str) 
evalRealExp (Integer n) _ _ str = (Float $ fromIntegral n, str) 

evalRealExp (Op1 "-" e) scope table str =  (Float $ - toFloat val, newStr)
  where (val, newStr) = evalRealExp e scope table str

-- Addition
evalRealExp (Op2 "+" e1 e2) scope table str = (Float (toFloat val1 + toFloat val2), str1 ++ str2)
  where (val1, str1) = evalRealExp e1 scope table str
        (val2, str2) = evalRealExp e2 scope table str
-- Subtraction
evalRealExp (Op2 "-" e1 e2) scope table str = (Float (toFloat val1 - toFloat val2), str1 ++ str2)
  where (val1, str1) = evalRealExp e1 scope table str
        (val2, str2) = evalRealExp e2 scope table str
-- Multiplcation
evalRealExp (Op2 "*" e1 e2) scope table str = (Float (toFloat val1 * toFloat val2), str1 ++ str2)
  where (val1, str1) = evalRealExp e1 scope table str
        (val2, str2) = evalRealExp e2 scope table str
-- Division
evalRealExp (Op2 "/" e1 e2) scope table str = (Float (toFloat val1 / toFloat val2), str1 ++ str2)
  where (val1, str1) = evalRealExp e1 scope table str
        (val2, str2) = evalRealExp e2 scope table str

-- square root
evalRealExp (Sqrt e) scope table str = (Float (sqrt $ toFloat val), newStr)
  where (val, newStr) = evalRealExp e scope table str
-- sin
evalRealExp (Sin e) scope table str = (Float (sin $ toFloat val), newStr)
  where (val, newStr) = evalRealExp e scope table str
-- cos
evalRealExp (Cos e) scope table str = (Float (cos $ toFloat val), newStr)
  where (val, newStr) = evalRealExp e scope table str
-- natural log
evalRealExp (Ln e) scope table str = (Float (log $ toFloat val), newStr)
  where (val, newStr) = evalRealExp e scope table str
-- exp
evalRealExp (Exp e) scope table str = (Float (exp $ toFloat val), newStr)
  where (val, newStr) = evalRealExp e scope table str

-- Variable
evalRealExp (VarReal name) scope _ str = (getVal name scope, str)

-- Function
evalRealExp (FuncReal name params) scope table str = (getVal name newScope, newStr)
  where (newStr, newScope) = executeFunc name params scope table


-- Evaluate general expressions
evalExp :: GenExp -> Scope -> FuncTable -> (ValueT, String)
evalExp (FloatExp exp) scope table = evalRealExp exp scope table ""
evalExp (BExp exp) scope table = evalBoolExp exp scope table ""

printList :: [Param] -> Scope -> FuncTable -> String
printList [exp] scope table = case exp of 
  RealP e -> let (val, str) = evalExp (FloatExp e) scope table in str ++ toString val ++ " "
  BoolP e -> let (val, str) =  evalExp (BExp e) scope table in str ++ toString val  ++ " "
  StrP e -> let (val, str) =  evalExp (FloatExp (VarReal e)) scope table in str ++ toString val ++ " "
printList (exp : tl) scope table = 
  printList [exp] scope table ++ printList tl scope table 

-- Evaluate statement
parseStmt :: Statement -> Scope -> FuncTable -> (String, Scope)
-- print statement
parseStmt (Print exp) scope table = case val of
    Float f -> (str ++ show f ++ "\n", scope)
    Bool b -> (str ++ show b ++ "\n", scope)
    where (val, str) = evalExp exp scope table
-- print statement with more than 1 argument
parseStmt (PrintList list) scope table = 
  ((printList list scope table) ++ "\n", scope)
-- print new line
parseStmt PrintNewLine scope table = ("\n", scope)
-- assignment
parseStmt (Assign name exp) scope table = (str, assignVal name val scope)
  where (val, str) = evalExp exp scope table
-- if else statement
parseStmt (IfElse boolExp s1 s2) scope table = case val of
  Bool True -> let (newStr, newScope) = parseStmt s1 scope table in (str ++ newStr, newScope)
  Bool False -> let (newStr, newScope) = parseStmt s2 scope table in (str ++ newStr, newScope)
  where (val, str) = evalExp (BExp boolExp) scope table
-- if statement
parseStmt (If boolExp stmt) scope table = case val of
  Bool True -> let (newStr, newScope) = parseStmt stmt scope table in (str ++ newStr, newScope)
  Bool False -> (str, scope)
  where (val, str) = evalExp (BExp boolExp) scope table
-- case statements
parseStmt (Case var caseStmts stmts) scope table = case b of
  True -> result
  False -> processBody stmts scope table
  where (result, b) = executeCase var caseStmts scope table False

parseStmt (Block stmts) scope table = processBody stmts scope table

parseStmt (While boolExp stmt) scope table = executeWhile boolExp stmt scope "" table

parseStmt (ForUp name exp1 exp2 stmt) scope table = 
  let (val, str) = evalExp (FloatExp exp1) scope table in 
  let newTable = assignVal name val scope in
    executeFor "+" "<=" name exp2 stmt newTable str table 

parseStmt (ForDown name exp1 exp2 stmt) scope table = 
  let (val, str) = evalExp (FloatExp exp1) scope table in 
  let newTable = assignVal name val scope in
    executeFor "-" ">=" name exp2 stmt newTable str table

parseStmt (FuncCall name vars) scope table = executeFunc name vars scope table 

parseStmt _ scope _ = ("There is a problem", scope)


-- Execute for loop
executeFor :: String -> String -> String -> RealExp -> Statement -> Scope -> String -> FuncTable -> (String, Scope)
executeFor op1 op2 var exp stmt scope str table = case value of
  Bool True -> let (newStr, newScope1) = parseStmt stmt scope table in 
               case equal of 
                    Bool True -> (str ++ newStr, newScope1)
                    Bool False -> let (val, _) = evalExp (FloatExp (Op2 op1 (Real varValue) (Integer 1))) newScope1 table in
                      let newScope2 = assignVal var val newScope1
                      in executeFor op1 op2 var exp stmt newScope2 (str ++ newStr) table
  Bool False -> (str, scope)
  where (value, _) = evalExp (BExp (Comp op2 (VarReal var) exp)) scope table
        (equal, _) = evalExp (BExp (Comp "=" (VarReal var) exp)) scope table
        (val, _) = evalExp (FloatExp (VarReal var)) scope table
        varValue = toFloat val

-- Execute case statements 
executeCase :: String -> [CaseStmt] -> Scope -> FuncTable -> Bool -> ((String, Scope), Bool)
executeCase var [(Check exp stmt)] scope table b
  | target == cur = (parseStmt stmt scope table, True)
  | otherwise = (("", scope), or [b, False])
  where target = getVal var scope
        (cur, _) = evalExp exp scope table
executeCase var (stmt : tl) scope table b = ((str1 ++ str2, newScope2), b2)
  where ((str1, newScope1), b1) = executeCase var [stmt] scope table b 
        ((str2, newScope2), b2) = executeCase var tl newScope1 table (or [b, b1]) 

-- Execute while loop
executeWhile :: BoolExp -> Statement -> Scope -> String -> FuncTable -> (String, Scope)
executeWhile boolExp stmt scope str table = case value of 
  Bool True -> let (newStr, newTable) = processBody [stmt] scope table in
               executeWhile boolExp stmt newTable (str ++ newStr) table
  Bool False -> (str, scope)
  where (value, _) = evalExp (BExp boolExp) scope table

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
  where (value, _) = evalRealExp param scope table ""
addParams [name] [BoolP param] scope table = assignVal name value scope
  where (value, _) = evalBoolExp param scope table ""
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
