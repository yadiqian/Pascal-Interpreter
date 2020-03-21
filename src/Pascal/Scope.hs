module Pascal.Scope
(
    buildTable,
    parseDef,
    getVal,
    assignVal
) 
where

import Pascal.Data

import qualified Data.Map as Map

parseDef :: Definition -> [(String, ValueT)] -> [(String, ValueT)]
parseDef (VarDef [name] varType) list = case varType of
  BOOL -> (name, (Bool False)) : list
  REAL -> (name, (Float 0.0)) : list
parseDef (VarDef (name : tl) varType) list = parseDef (VarDef [name] varType) list 
  ++ parseDef (VarDef tl varType) list

buildTable :: [(String, ValueT)] -> Table
buildTable list = Map.fromList list

lookupVal :: String -> Table -> Maybe ValueT
lookupVal = Map.lookup

assignVal :: String -> ValueT -> Table -> Table
assignVal name value table = Map.insert name value table

getVal :: String -> Table -> ValueT
getVal name table = case (lookupVal name table) of 
  Just n -> n
  Nothing -> Float 0.0
