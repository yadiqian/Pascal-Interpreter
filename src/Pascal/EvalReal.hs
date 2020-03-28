module Pascal.EvalReal 
(
    -- evalRealExp
) 
where

import Pascal.Data
import Pascal.Scope
import Pascal.Function

-- Evaluate real expressions
-- evalRealExp :: RealExp -> Scope -> FuncTable -> ValueT
-- evalRealExp (Real n) _ _ = Float n 
-- evalRealExp (Integer n) _ _ = Float $ fromIntegral n 

-- evalRealExp (Op1 "-" e) scope table =  Float $ - toFloat (evalRealExp e scope table)

-- -- Addition
-- evalRealExp (Op2 "+" e1 e2) scope table = 
--   Float (toFloat (evalRealExp e1 scope table) + toFloat (evalRealExp e2 scope table))
-- -- Subtraction
-- evalRealExp (Op2 "-" e1 e2) scope table = 
--   Float (toFloat (evalRealExp e1 scope table) - toFloat (evalRealExp e2 scope table))
-- -- Multiplcation
-- evalRealExp (Op2 "*" e1 e2) scope table = 
--   Float (toFloat (evalRealExp e1 scope table) * toFloat (evalRealExp e2 scope table))
-- -- Division
-- evalRealExp (Op2 "/" e1 e2) scope table = 
--   Float (toFloat (evalRealExp e1 scope table) / toFloat (evalRealExp e2 scope table))

-- -- square root
-- evalRealExp (Sqrt e) scope table = Float (sqrt $ toFloat $ evalRealExp e scope table)
-- -- sin
-- evalRealExp (Sin e) scope table = Float (sin $ toFloat $ evalRealExp e scope table)
-- -- cos
-- evalRealExp (Cos e) scope table = Float (cos $ toFloat $ evalRealExp e scope table)
-- -- natural log
-- evalRealExp (Ln e) scope table = Float (log $ toFloat $ evalRealExp e scope table)
-- -- exp
-- evalRealExp (Exp e) scope table = Float (exp $ toFloat $ evalRealExp e scope table)

-- -- Variable
-- evalRealExp (VarReal name) scope _ = getVal name scope

-- Function
-- evalRealExp (FuncReal name params) scope table = getVal name newScope
--   where (str, newScope) = executeFunc name params scope table
