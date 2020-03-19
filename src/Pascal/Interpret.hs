module Pascal.Interpret 
(
    interpret
)
where

import Pascal.Data
import Pascal.EvalReal
import Pascal.EvalBool

-- TODO: define auxiliary functions to aid interpretation
-- Feel free to put them here or in different modules
-- Hint: write separate evaluators for numeric and
-- boolean expressions and for statements

-- make sure you write test unit cases for all functions

-- Evaluate general expressions
evalExp :: GenExp -> String
evalExp (FloatExp exp) = show $ evalRealExp exp
evalExp (BExp exp) = show $ evalBoolExp exp

-- Evaluate statement
parseStmt :: Statement -> String
parseStmt (Print exp) = evalExp exp ++ "\n"

-- evaluate program statements
interpret :: Program -> String
interpret ([stmt]) = parseStmt stmt
interpret (stmt : tl) = interpret [stmt] ++ interpret tl

interpret _ = "Not implemented"