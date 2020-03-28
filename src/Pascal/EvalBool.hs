module Pascal.EvalBool
(
    -- evalBoolExp
) 
where

import Pascal.Data
import Pascal.EvalReal
import Pascal.Scope
import Pascal.Function

-- Evaluate boolean expression
-- evalBoolExp :: BoolExp -> Scope -> FuncTable -> ValueT
-- evalBoolExp True_C _ _ = Bool True 
-- evalBoolExp False_C _ _ = Bool False 

-- -- and
-- evalBoolExp (OpB "and" e1 e2) scope table = 
--   Bool (and [toBool $ evalBoolExp e1 scope table, toBool $ evalBoolExp e2 scope table])
-- -- or
-- evalBoolExp (OpB "or" e1 e2) scope table = 
--   Bool (or [toBool $ evalBoolExp e1 scope table, toBool $ evalBoolExp e2 scope table])
-- -- xor
-- evalBoolExp (OpB "xor" e1 e2) scope table = 
--   Bool (toBool (evalBoolExp e1 scope table) /= toBool (evalBoolExp e2 scope table))
-- -- not
-- evalBoolExp (Not e) scope table = 
--   Bool (not $ toBool $ evalBoolExp e scope table)

-- -- Evaluate comparison
-- evalBoolExp (Comp ">" e1 e2) scope table = 
--   Bool (toFloat (evalRealExp e1 scope table) > toFloat (evalRealExp e2 scope table))

-- evalBoolExp (Comp "<" e1 e2) scope table = 
--   Bool (toFloat (evalRealExp e1 scope table) < toFloat (evalRealExp e2 scope table))
  
-- evalBoolExp (Comp "=" e1 e2) scope table = 
--   Bool (toFloat (evalRealExp e1 scope table) == toFloat (evalRealExp e2 scope table))

-- evalBoolExp (Comp ">=" e1 e2) scope table = 
--   Bool (toFloat (evalRealExp e1 scope table) >= toFloat (evalRealExp e2 scope table))

-- evalBoolExp (Comp "<=" e1 e2) scope table = 
--   Bool (toFloat (evalRealExp e1 scope table) <= toFloat (evalRealExp e2 scope table))

-- evalBoolExp (Comp "<>" e1 e2) scope table = 
--   Bool (toFloat (evalRealExp e1 scope table) /= toFloat (evalRealExp e2 scope table))

-- -- Variable
-- evalBoolExp (VarBool name) scope _ = getVal name scope

-- Function
-- evalRealExp (FuncBool name params) scope table = getVal name newScope
--   where (str, newScope) = executeFunc name params scope table
