module Pascal.EvalBool
(
    evalBoolExp
) 
where

import Pascal.Data
import Pascal.EvalReal

-- Evaluate boolean expression
evalBoolExp :: BoolExp -> Bool
evalBoolExp True_C = True
evalBoolExp False_C = False

-- and
evalBoolExp (OpB "and" e1 e2) = and [evalBoolExp e1, evalBoolExp e2]
-- or
evalBoolExp (OpB "or" e1 e2) = or [evalBoolExp e1, evalBoolExp e2]
-- xor
evalBoolExp (OpB "xor" e1 e2) = evalBoolExp e1 /= evalBoolExp e2
-- not
evalBoolExp (Not e) = not $ evalBoolExp e

-- Evaluate comparison
evalBoolExp (Comp ">" e1 e2) = evalRealExp e1 > evalRealExp e2
evalBoolExp (Comp "<" e1 e2) = evalRealExp e1 < evalRealExp e2
evalBoolExp (Comp "=" e1 e2) = evalRealExp e1 == evalRealExp e2
evalBoolExp (Comp ">=" e1 e2) = evalRealExp e1 >= evalRealExp e2
evalBoolExp (Comp "<=" e1 e2) = evalRealExp e1 <= evalRealExp e2
evalBoolExp (Comp "<>" e1 e2) = evalRealExp e1 /= evalRealExp e2