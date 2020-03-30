import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Pascal.Function
import Pascal.Data
import Pascal.Scope

import qualified Data.Map as Map

main :: IO ()

scope :: [Table]
scope = [Map.fromList [("a", (Bool True)), ("b", (Float 1.0))], Map.fromList [("c", Float 15.5)]]

table :: FuncTable
table = Map.fromList [
  ("funcB", ([], [], [], [Assign "funcB" (BExp True_C)])),
  ("funcR", ([], [], [], [Print (FloatExp (Real 3.0)), Assign "funcR" (FloatExp $ Real (-0.7))]))
  ]

main = hspec $ do
  describe "endScope" $ do
    it "Copies referenced variables" $ do
      endScope [StrP "sum"] ["b"] scope 
        `shouldBe` [Map.fromList [("a", (Bool True)), ("b", (Float 1.0))], Map.fromList [("c", Float 15.5), ("sum", Float 1.0)]]
      endScope [StrP "d"] ["a"] scope 
        `shouldBe` [Map.fromList [("a", (Bool True)), ("b", (Float 1.0))], Map.fromList [("c", Float 15.5), ("d", Bool True)]]
      endScope [StrP "d", StrP "e"] ["a", "b"] scope 
        `shouldBe` [Map.fromList [("a", (Bool True)), ("b", (Float 1.0))], Map.fromList [("c", Float 15.5), ("d", Bool True), ("e", Float 1.0)]]
  
  describe "lastN" $ do
    it "Keeps the last N elements in list" $ do
      lastN 0 [1, 2] `shouldBe` []
      lastN 1 [1, 2] `shouldBe` [2]
      lastN 2 [1, 2] `shouldBe` [1, 2]
      lastN 3 [1, 2] `shouldBe` [1, 2]
    
  describe "refsToStrs" $ do
    it "Converts var defs to strings" $ do
      refsToStrs [] [] `shouldBe` []
      refsToStrs [VarDef ["age"] REAL] [] `shouldBe` ["age"]
      refsToStrs [VarDef ["age"] REAL] ["name", "gender"] `shouldBe` ["name", "gender", "age"]
      refsToStrs [VarDef ["age", "gender"] REAL, VarDef ["name"] BOOL] [] 
        `shouldBe` ["age", "gender", "name"]

  describe "storeDef" $ do
    it "Stores variable definitions" $ do
      storeDef ["age", "gender"] REAL [emptyScope] 
        `shouldBe` [Map.fromList [("age", Float 0.0), ("gender", Float 0.0)]]
      storeDef ["status"] BOOL [emptyScope] 
        `shouldBe` [Map.fromList [("status", Bool False)]]