import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Pascal.Scope
import Pascal.Data

import qualified Data.Map as Map

main :: IO ()

scope :: [Table]
scope = [Map.fromList [("a", (Bool True)), ("b", (Float 1.0))]]

table :: FuncTable
table = Map.fromList [
  ("funcB", ([], [], [], [Assign "funcB" (BExp True_C)])),
  ("funcR", ([], [], [], [Print (FloatExp (Real 3.0)), Assign "funcR" (FloatExp $ Real (-0.7))]))
  ]

main = hspec $ do
  describe "addFunc" $ do
    it "Adds a new function to the function table" $ do
      addFunc "func" [] [] [] [Print (FloatExp (Real 3.0))] funcTable 
        `shouldBe` Map.fromList [("func", ([], [], [], [Print (FloatExp (Real 3.0))]))]
  describe "addVar" $ do
    it "Adds a new variable to the scope" $ do
      addVar "age" (Float 20.0) scope `shouldBe` [Map.insert "age" (Float 20.0) (head scope)]
      addVar "student" (Bool True) scope `shouldBe` [Map.insert "student" (Bool True) (head scope)]
  describe "assignVal" $ do
    it "Assigns value to a variable" $ do
      assignVal "b" (Float (-8.0)) scope `shouldBe` [Map.fromList [("a", (Bool True)), ("b", (Float (-8.0)))]]
  describe "getVal" $ do
    it "Gets the value of the variable" $ do
      getVal "a" scope `shouldBe` Bool True
      getVal "b" scope `shouldBe` Float 1.0
    it "Errors when variable not in scope" $ do
      evaluate(getVal "a" []) `shouldThrow` errorCall "Variable not in scope"
      evaluate(getVal "c" scope) `shouldThrow` errorCall "Variable not in scope"

  describe "getFunc" $ do
    it "Gets the function" $ do
      getFunc "funcR" table `shouldBe` ([], [], [], [Print (FloatExp (Real 3.0)), Assign "funcR" (FloatExp $ Real (-0.7))])
    it "Errors when function or procedure not in scope" $ do
      evaluate(getFunc "func" table) `shouldThrow` errorCall "Function or procedure not found"