module Pascal.Scope
(
    buildTable,
    parseDef,
    getReal,
    getBool,
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

getReal :: String -> Table -> Float
getReal name table = case (lookupVal name table) of 
    Just n -> case n of 
      Float n -> n
      Bool n -> 0.0
    Nothing -> 0.0

getBool :: String -> Table -> Bool
getBool name table = case (lookupVal name table) of 
    Just n -> case n of 
      Bool n -> n
      Float n -> False
    Nothing -> False
