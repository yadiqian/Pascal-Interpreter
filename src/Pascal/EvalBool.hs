module Pascal.EvalBool
(
    evalBoolExp
) 
where

import Pascal.Data
import Pascal.EvalReal
import Pascal.Scope

-- Evaluate boolean expression
evalBoolExp :: BoolExp -> Table -> ValueT
evalBoolExp True_C _ = Bool True 
evalBoolExp False_C _ = Bool False 

-- and
evalBoolExp (OpB "and" e1 e2) table = 
  Bool (and [toBool $ evalBoolExp e1 table, toBool $ evalBoolExp e2 table])
-- or
evalBoolExp (OpB "or" e1 e2) table = 
  Bool (or [toBool $ evalBoolExp e1 table, toBool $ evalBoolExp e2 table])
-- xor
evalBoolExp (OpB "xor" e1 e2) table = 
  Bool (toBool (evalBoolExp e1 table) /= toBool (evalBoolExp e2 table))
-- not
evalBoolExp (Not e) table = 
  Bool (not $ toBool $ evalBoolExp e table)

-- Evaluate comparison
evalBoolExp (Comp ">" e1 e2) table = 
  Bool (toFloat (evalRealExp e1 table) > toFloat (evalRealExp e2 table))

evalBoolExp (Comp "<" e1 e2) table = 
  Bool (toFloat (evalRealExp e1 table) < toFloat (evalRealExp e2 table))
  
evalBoolExp (Comp "=" e1 e2) table = 
  Bool (toFloat (evalRealExp e1 table) == toFloat (evalRealExp e2 table))

evalBoolExp (Comp ">=" e1 e2) table = 
  Bool (toFloat (evalRealExp e1 table) >= toFloat (evalRealExp e2 table))

evalBoolExp (Comp "<=" e1 e2) table = 
  Bool (toFloat (evalRealExp e1 table) <= toFloat (evalRealExp e2 table))
  
evalBoolExp (Comp "<>" e1 e2) table = 
  Bool (toFloat (evalRealExp e1 table) /= toFloat (evalRealExp e2 table))

-- Variable
evalBoolExp (VarBool name) table = getVal name table
