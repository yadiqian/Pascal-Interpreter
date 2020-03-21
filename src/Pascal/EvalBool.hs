module Pascal.EvalBool
(
    evalBoolExp
) 
where

import Pascal.Data
import Pascal.EvalReal
import Pascal.Scope

-- Evaluate boolean expression
evalBoolExp :: BoolExp -> Table -> Bool
evalBoolExp True_C _ = True 
evalBoolExp False_C _ = False 

-- and
evalBoolExp (OpB "and" e1 e2) table = and [evalBoolExp e1 table, evalBoolExp e2 table]
-- or
evalBoolExp (OpB "or" e1 e2) table = or [evalBoolExp e1 table, evalBoolExp e2 table]
-- xor
evalBoolExp (OpB "xor" e1 e2) table = evalBoolExp e1 table /= evalBoolExp e2 table
-- not
evalBoolExp (Not e) table = not $ evalBoolExp e table

-- Evaluate comparison
evalBoolExp (Comp ">" e1 e2) table = evalRealExp e1 table > evalRealExp e2 table
evalBoolExp (Comp "<" e1 e2) table = evalRealExp e1 table < evalRealExp e2 table
evalBoolExp (Comp "=" e1 e2) table = evalRealExp e1 table == evalRealExp e2 table
evalBoolExp (Comp ">=" e1 e2) table = evalRealExp e1 table >= evalRealExp e2 table
evalBoolExp (Comp "<=" e1 e2) table = evalRealExp e1 table <= evalRealExp e2 table
evalBoolExp (Comp "<>" e1 e2) table = evalRealExp e1 table /= evalRealExp e2 table

-- Variable
evalBoolExp (VarBool name) table = case getVal name table of
  Bool b -> b
  otherwise -> False