module Pascal.Function where 

import Pascal.Data
import Pascal.Scope

-- Move vars from current scope
endScope :: [Param] -> [String] -> Scope -> Scope
endScope [StrP name] [] (scope : tl) = (scope : assignVal name value tl)
  where value = getVal name [scope]
endScope [StrP name] [ref] (scope : tl) = (scope : assignVal name value tl)
  where value = getVal ref [scope]
endScope [RealP (VarReal name)] [ref] (scope : tl) = (scope : assignVal name value tl)
  where value = getVal ref [scope]
endScope [BoolP (VarBool name)] [ref] (scope : tl) = (scope : assignVal name value tl)
  where value = getVal ref [scope]
endScope (name : names) (ref : refs) scope = endScope names refs newScope
  where newScope = endScope [name] [ref] scope

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

refsToStrs :: [VarDef] -> [String] -> [String]
refsToStrs [] [] = []
refsToStrs [VarDef vars varType] strs = strs ++ vars
refsToStrs (def : tl) strs =  refsToStrs tl (refsToStrs [def] strs)

-- Store variable definitions
storeDef :: [String] -> VType -> Scope -> Scope
storeDef [var] varType scope = case varType of
  BOOL -> addVar var (Bool False) scope
  REAL -> addVar var (Float 0.0) scope
storeDef (var : tl) varType scope = storeDef tl varType newScope
  where newScope = storeDef [var] varType scope
