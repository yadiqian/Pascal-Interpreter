module Pascal.EvalReal 
(
    evalRealExp
) 
where

import Pascal.Data
import Pascal.Scope

-- Evaluate real expressions
evalRealExp :: RealExp -> Table -> ValueT
evalRealExp (Real n) _ = Float n 
evalRealExp (Integer n) _ = Float $ fromIntegral n 

evalRealExp (Op1 "-" e) table =  Float $ - toFloat (evalRealExp e table)

-- Addition
evalRealExp (Op2 "+" e1 e2) table = 
  Float (toFloat (evalRealExp e1 table) + toFloat (evalRealExp e2 table))
-- Subtraction
evalRealExp (Op2 "-" e1 e2) table = 
  Float (toFloat (evalRealExp e1 table) - toFloat (evalRealExp e2 table))
-- Multiplcation
evalRealExp (Op2 "*" e1 e2) table = 
  Float (toFloat (evalRealExp e1 table) * toFloat (evalRealExp e2 table))
-- Division
evalRealExp (Op2 "/" e1 e2) table = 
  Float (toFloat (evalRealExp e1 table) / toFloat (evalRealExp e2 table))

-- Variable
evalRealExp (VarReal name) table = getVal name table

-- square root
evalRealExp (Sqrt e) table = Float (sqrt $ toFloat $ evalRealExp e table)
-- sin
evalRealExp (Sin e) table = Float (sin $ toFloat $ evalRealExp e table)
-- cos
evalRealExp (Cos e) table = Float (cos $ toFloat $ evalRealExp e table)
-- natural log
evalRealExp (Ln e) table = Float (log $ toFloat $ evalRealExp e table)
-- exp
evalRealExp (Exp e) table = Float (exp $ toFloat $ evalRealExp e table)
