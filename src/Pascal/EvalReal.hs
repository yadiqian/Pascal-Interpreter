module Pascal.EvalReal 
(
    evalRealExp
) 
where

import Pascal.Data

-- Evaluate real expressions
evalRealExp :: RealExp -> Float
evalRealExp (Real n) = n
evalRealExp (Integer n) = fromIntegral n

evalRealExp (Op1 "-" (Integer n)) =  - fromIntegral n

-- Addition
evalRealExp (Op2 "+" e1 e2) = evalRealExp e1 + evalRealExp e2
-- Subtraction
evalRealExp (Op2 "-" e1 e2) = evalRealExp e1 - evalRealExp e2
-- Multiplcation
evalRealExp (Op2 "*" e1 e2) = evalRealExp e1 * evalRealExp e2
-- Division
evalRealExp (Op2 "/" e1 e2) = evalRealExp e1 / evalRealExp e2
