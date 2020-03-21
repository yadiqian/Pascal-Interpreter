module Pascal.EvalReal 
(
    evalRealExp
) 
where

import Pascal.Data
import Pascal.Scope

-- Evaluate real expressions
evalRealExp :: RealExp -> Table -> Float
evalRealExp (Real n) _ = n 
evalRealExp (Integer n) _ = fromIntegral n 

evalRealExp (Op1 "-" (Integer n)) _ =  - fromIntegral n
evalRealExp (Op1 "-" (Real n)) _ =  -n

-- Addition
evalRealExp (Op2 "+" e1 e2) table = evalRealExp e1 table + evalRealExp e2 table
-- Subtraction
evalRealExp (Op2 "-" e1 e2) table = evalRealExp e1 table - evalRealExp e2 table
-- Multiplcation
evalRealExp (Op2 "*" e1 e2) table = evalRealExp e1 table * evalRealExp e2 table
-- Division
evalRealExp (Op2 "/" e1 e2) table = evalRealExp e1 table / evalRealExp e2 table

-- Variable
evalRealExp (VarReal name) table = getReal name table

-- square root
evalRealExp (Sqrt e) table = sqrt $ evalRealExp e table
-- sin
evalRealExp (Sin e) table = sin $ evalRealExp e table
-- cos
evalRealExp (Cos e) table = cos $ evalRealExp e table
-- natural log
evalRealExp (Ln e) table = log $ evalRealExp e table
-- exp
evalRealExp (Exp e) table = exp $ evalRealExp e table
