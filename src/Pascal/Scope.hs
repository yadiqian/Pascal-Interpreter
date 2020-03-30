module Pascal.Scope
(
    assignVal,
    addVar,
    funcTable,
    getVal,
    addFunc,
    getFunc,
    emptyScope
) 
where

import Pascal.Data

import qualified Data.Map as Map

funcTable :: FuncTable
funcTable = Map.empty

emptyScope :: Table
emptyScope = Map.empty

addFunc :: String -> [VarDef] -> [VarDef] -> [Definition] -> [Statement] -> FuncTable -> FuncTable
addFunc name vars varRefs defs stmts table = Map.insert name (vars, varRefs, defs, stmts) table

addVar :: String -> ValueT -> Scope -> Scope
addVar name val [table] = [Map.insert name val table] 
addVar name val (table : tl) = (Map.insert name val table) : tl

lookupVal :: String -> Table -> Maybe ValueT
lookupVal = Map.lookup

lookupFunc :: String -> FuncTable -> Maybe ([VarDef], [VarDef], [Definition], [Statement])
lookupFunc = Map.lookup

assignVal :: String -> ValueT -> Scope -> Scope
assignVal name value [scope] = [Map.insert name value scope]
assignVal name value (scope : tl) = (Map.insert name value scope) : tl
 
getVal :: String -> Scope -> ValueT
getVal name [] = error("Variable not in scope")
getVal name [scope] = case (lookupVal name scope) of 
  Just n -> n
  Nothing -> error("Variable not in scope")
getVal name (scope : tl) = case (lookupVal name scope) of 
  Just n -> n
  Nothing -> getVal name tl

getFunc :: String -> FuncTable -> ([VarDef], [VarDef], [Definition], [Statement])
getFunc name table = case (lookupFunc name table) of 
  Just val -> val
  Nothing -> error("Function or procedure not found")
