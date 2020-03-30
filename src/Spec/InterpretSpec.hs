import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Pascal.Interpret
import Pascal.Scope
import Pascal.Data

import qualified Data.Map as Map

main :: IO ()

scope :: [Table]
scope = [Map.fromList [("a", (Bool True)), ("b", (Float 0.5))]]

scopeInsert :: [Table]
scopeInsert = [Map.fromList [("a", (Bool True)), ("b", (Float 0.5)), ("c", (Float (-1.1)))]]

table :: FuncTable
table = Map.fromList [
  ("funcB", ([], [], [], [Assign "funcB" (BExp True_C)])),
  ("funcR", ([], [], [], [Print (FloatExp (Real 3.0)), Assign "funcR" (FloatExp $ Real (-0.7))]))
  ]

main = hspec $ do
  describe "evalBoolExp" $ do
    it "returns true given true" $ do
        evalBoolExp True_C [] funcTable "" `shouldBe` (Bool True, "")
    it "returns false given false" $ do
        evalBoolExp False_C [] funcTable "" `shouldBe` (Bool False, "")
    it "AND 2 boolean expressions" $ do
        evalBoolExp (OpB "and" True_C False_C) [] funcTable "" `shouldBe` (Bool False, "")
        evalBoolExp (OpB "and" True_C True_C) [] funcTable "" `shouldBe` (Bool True, "")
        evalBoolExp (OpB "and" False_C False_C) [] funcTable "" `shouldBe` (Bool False, "")
    it "OR 2 boolean expressions" $ do
        evalBoolExp (OpB "or" True_C False_C) [] funcTable "" `shouldBe` (Bool True, "")
        evalBoolExp (OpB "or" True_C True_C) [] funcTable "" `shouldBe` (Bool True, "")
        evalBoolExp (OpB "or" False_C False_C) [] funcTable "" `shouldBe` (Bool False, "")
    it "XOR 2 boolean expressions" $ do
        evalBoolExp (OpB "xor" True_C False_C) [] funcTable "" `shouldBe` (Bool True, "")
        evalBoolExp (OpB "xor" True_C True_C) [] funcTable "" `shouldBe` (Bool False, "")
        evalBoolExp (OpB "xor" False_C False_C) [] funcTable "" `shouldBe` (Bool False, "")
    it "NOT boolean expression" $ do
        evalBoolExp (Not False_C) [] funcTable "" `shouldBe` (Bool True, "")
        evalBoolExp (Not True_C) [] funcTable "" `shouldBe` (Bool False, "")
    it "Evaluates greater than comparison" $ do
        evalBoolExp (Comp ">" (Real 5.0) (Real (-4.0))) [] funcTable "" `shouldBe` (Bool True, "")
        evalBoolExp (Comp ">" (Real 5.0) (Integer 10)) [] funcTable "" `shouldBe` (Bool False, "")
        evalBoolExp (Comp ">" (Integer (-11)) (Real (-11.0))) [] funcTable "" `shouldBe` (Bool False, "")
        evalBoolExp (Comp ">" (Integer 3) (Integer 100)) [] funcTable "" `shouldBe` (Bool False, "")
    it "Evaluates less than comparison" $ do
        evalBoolExp (Comp "<" (Real 5.0) (Real (-4.0))) [] funcTable "" `shouldBe` (Bool False, "")
        evalBoolExp (Comp "<" (Integer 5) (Real (-4.0))) [] funcTable "" `shouldBe` (Bool False, "")
        evalBoolExp (Comp "<" (Real 3.0) (Integer 100)) [] funcTable "" `shouldBe` (Bool True, "")
        evalBoolExp (Comp "<" (Integer 3) (Integer 100)) [] funcTable "" `shouldBe` (Bool True, "")
    it "Evaluates equal to comparison" $ do
        evalBoolExp (Comp "=" (Real 5.0) (Real (-4.0))) [] funcTable "" `shouldBe` (Bool False, "")
        evalBoolExp (Comp "=" (Real 5.0) (Integer 5)) [] funcTable "" `shouldBe` (Bool True, "")
        evalBoolExp (Comp "=" (Integer 5) (Real (-4.0))) [] funcTable "" `shouldBe` (Bool False, "")
        evalBoolExp (Comp "=" (Integer 4) (Integer 4)) [] funcTable "" `shouldBe` (Bool True, "")
    it "Evaluates greater than or equal to comparison" $ do
        evalBoolExp (Comp ">=" (Real 5.0) (Real (-4.0))) [] funcTable "" `shouldBe` (Bool True, "")
        evalBoolExp (Comp ">=" (Real 5.0) (Integer 5)) [] funcTable "" `shouldBe` (Bool True, "")
        evalBoolExp (Comp ">=" (Integer 5) (Real 8.0)) [] funcTable "" `shouldBe` (Bool False, "")
        evalBoolExp (Comp ">=" (Integer 4) (Integer 4)) [] funcTable "" `shouldBe` (Bool True, "")
    it "Evaluates less than or equal to comparison" $ do
        evalBoolExp (Comp "<=" (Real 5.0) (Real (-4.0))) [] funcTable "" `shouldBe` (Bool False, "")
        evalBoolExp (Comp "<=" (Real 5.0) (Integer 5)) [] funcTable "" `shouldBe` (Bool True, "")
        evalBoolExp (Comp "<=" (Integer 5) (Real 8.0)) [] funcTable "" `shouldBe` (Bool True, "")
        evalBoolExp (Comp "<=" (Integer 4) (Integer 4)) [] funcTable "" `shouldBe` (Bool True, "")
    it "Evaluates not equal to comparison" $ do
        evalBoolExp (Comp "<>" (Real 5.0) (Real (-4.0))) [] funcTable "" `shouldBe` (Bool True, "")
        evalBoolExp (Comp "<>" (Real 5.0) (Integer 5)) [] funcTable "" `shouldBe` (Bool False, "")
        evalBoolExp (Comp "<>" (Integer 5) (Real 8.0)) [] funcTable "" `shouldBe` (Bool True, "")
        evalBoolExp (Comp "<>" (Integer 4) (Integer 4)) [] funcTable "" `shouldBe` (Bool False, "")
    it "Evaluates variable" $ do
        evalBoolExp (VarBool "a") scope funcTable "" `shouldBe` (Bool True, "")
        evalBoolExp (VarBool "b") scope funcTable "" `shouldBe` (Float 0.5, "")
    it "Evaluates function" $ do
        evalBoolExp (FuncBool "funcB" []) scope table "" `shouldBe` (Bool True, "")
        evalBoolExp (FuncBool "funcR" []) scope table "" `shouldBe` (Float (-0.7), "3.0\n")

  describe "evalRealExp" $ do
    it "returns float given real" $ do
        evalRealExp (Real 4.5) [] funcTable "" `shouldBe` (Float 4.5, "")
    it "returns float given integer" $ do
        evalRealExp (Integer 0) [] funcTable "" `shouldBe` (Float 0.0, "")
    it "Evaluates negative expression" $ do
        evalRealExp (Op1 "-" (Real 3.3)) [] funcTable "" `shouldBe` (Float (-3.3), "")
        evalRealExp (Op1 "-" (Integer 3)) [] funcTable "" `shouldBe` (Float (-3), "")
    it "Evaluates addition" $ do
        evalRealExp (Op2 "+" (Real 3.3) (Real 4.4)) [] funcTable "" `shouldBe` (Float 7.7, "")
        evalRealExp (Op2 "+" (Real 3.3) (Integer (-3))) [] funcTable "" `shouldBe` (Float 0.29999995, "")
        evalRealExp (Op2 "+" (Integer 8) (Real 4.4)) [] funcTable "" `shouldBe` (Float 12.4, "")
        evalRealExp (Op2 "+" (Integer 3) (Integer 4)) [] funcTable "" `shouldBe` (Float 7, "")
        evalRealExp (Op2 "+" (Cos (Integer 0)) (Cos (Real pi))) [] funcTable "" `shouldBe` (Float 0.0, "")
    it "Evaluates subtraction" $ do
        evalRealExp (Op2 "-" (Real 3.3) (Real 4.4)) [] funcTable "" `shouldBe` (Float (-1.1000001), "")
        evalRealExp (Op2 "-" (Real 3.3) (Integer (-3))) [] funcTable "" `shouldBe` ((Float 6.3), "")
        evalRealExp (Op2 "-" (Integer 8) (Real 4.4)) [] funcTable "" `shouldBe` (Float 3.6, "")
        evalRealExp (Op2 "-" (Integer 3) (Integer 4)) [] funcTable "" `shouldBe` (Float (-1), "")
    it "Evaluates multiplication" $ do
        evalRealExp (Op2 "*" (Real 0.3) (Real 4.0)) [] funcTable "" `shouldBe` (Float 1.2, "")
        evalRealExp (Op2 "*" (Real 3.3) (Integer (-3))) [] funcTable "" `shouldBe` (Float (-9.9), "")
        evalRealExp (Op2 "*" (Integer 0) (Real 4.4)) [] funcTable "" `shouldBe` (Float 0.0, "")
        evalRealExp (Op2 "*" (Integer 3) (Integer 4)) [] funcTable "" `shouldBe` (Float 12, "")

    context "/" $ do
      it "Evaluates division" $ do
          evalRealExp (Op2 "/" (Real 3.3) (Real 1.1)) [] funcTable "" `shouldBe` ((Float 3.0), "")
          evalRealExp (Op2 "/" (Real 1.0) (Integer (-10))) [] funcTable "" `shouldBe` ((Float (-0.1)), "")
          evalRealExp (Op2 "/" (Integer 0) (Real 4.4)) [] funcTable "" `shouldBe` ((Float 0.0), "")
          evalRealExp (Op2 "/" (Integer 36) (Integer 4)) [] funcTable "" `shouldBe` ((Float 9), "")         
      it "Errors on divisor as 0" $ do 
          evaluate (evalRealExp (Op2 "/" (Integer 36) (Integer 0)) [] funcTable "") `shouldThrow` errorCall "Cannot divide by 0"
          evaluate (evalRealExp (Op2 "/" (Integer 36) (Real 0.0)) [] funcTable "") `shouldThrow` errorCall "Cannot divide by 0"

    context "Sqrt" $ do
      it "Evaluates square root" $ do
          evalRealExp (Sqrt (Real 4.0)) [] funcTable "" `shouldBe` ((Float 2.0), "")
          evalRealExp (Sqrt (Integer 16)) [] funcTable "" `shouldBe` ((Float 4.0), "")
          evalRealExp (Sqrt (Integer 0)) [] funcTable "" `shouldBe` ((Float 0.0), "")
      it "Errors on negative expression" $ do
          evaluate (evalRealExp (Sqrt (Real (-4.0))) [] funcTable "") `shouldThrow` errorCall "Cannot square root negative numbers"
          evaluate (evalRealExp (Sqrt (Integer (-9))) [] funcTable "") `shouldThrow` errorCall "Cannot square root negative numbers"

    it "Evaluates sin" $ do
          evalRealExp (Sin (Integer 0)) [] funcTable "" `shouldBe` ((Float 0.0), "")
          evalRealExp (Sin (Real pi)) [] funcTable "" `shouldBe` ((Float (-8.742278e-8)), "")
    it "Evaluates cos" $ do
          evalRealExp (Cos (Integer 0)) [] funcTable "" `shouldBe` ((Float 1.0), "")
          evalRealExp (Cos (Real pi)) [] funcTable "" `shouldBe` ((Float (-1.0)), "")
    it "Evaluates natural log" $ do
          evalRealExp (Ln (Integer 1)) [] funcTable "" `shouldBe` ((Float 0.0), "")
          evalRealExp (Ln (Real (exp 1))) [] funcTable "" `shouldBe` ((Float 0.99999994), "")
    it "Evaluates exp" $ do
          evalRealExp (Exp (Integer 0)) [] funcTable "" `shouldBe` ((Float 1.0), "")
          evalRealExp (Exp (Real (3.3))) [] funcTable "" `shouldBe` ((Float (exp 3.3)), "")
    it "Evaluates variable" $ do
        evalRealExp (VarReal "a") scope funcTable "" `shouldBe` (Bool True, "")
        evalRealExp (VarReal "b") scope funcTable "" `shouldBe` (Float 0.5, "")
    it "Evaluates function" $ do
        evalRealExp (FuncReal "funcB" []) scope table "" `shouldBe` (Bool True, "")
        evalRealExp (FuncReal "funcR" []) scope table "" `shouldBe` (Float (-0.7), "3.0\n")

  describe "parseStmt" $ do
    it "Returns print string" $ do
      parseStmt (Print (FloatExp $ Real 1.5)) [] funcTable `shouldBe` ("1.5\n", [])
      parseStmt (Print (BExp True_C)) [] funcTable `shouldBe` ("True\n", [])
      parseStmt (Print (FloatExp $ VarReal "b")) scope funcTable `shouldBe` ("0.5\n", scope)
    it "Returns print string list" $ do
      parseStmt (PrintList [(RealP $ Real 1.5), (BoolP True_C), (StrP "b")]) scope funcTable `shouldBe` ("1.5 True 0.5 \n", scope)
    it "Returns a new line" $ do
      parseStmt PrintNewLine [] funcTable `shouldBe` ("\n", [])
    it "Assigns value to variable" $ do
      parseStmt (Assign "c" (FloatExp $ Real (-1.1))) scope funcTable `shouldBe` ("", scopeInsert)

    context "IfElse" $ do
      it "Executes if statement" $ do
        parseStmt (IfElse True_C (Print (BExp True_C)) (Print (BExp False_C))) [] funcTable `shouldBe` ("True\n", [])
        parseStmt (IfElse (Comp "=" (Real 5.0) (Real 5.0)) (Print (BExp True_C)) (Print (BExp False_C))) [] funcTable `shouldBe` ("True\n", [])
        parseStmt (IfElse (Comp "<>" (Real 5.0) (Real 5.1)) (Print (BExp True_C)) (Print (BExp False_C))) [] funcTable `shouldBe` ("True\n", [])
        parseStmt (IfElse (Comp ">=" (Real 6.0) (Real 5.0)) (Print (BExp True_C)) (Print (BExp False_C))) [] funcTable `shouldBe` ("True\n", [])
      it "Executes else statement" $ do
        parseStmt (IfElse False_C (Print (BExp True_C)) (Print (BExp False_C))) [] funcTable `shouldBe` ("False\n", [])
        parseStmt (IfElse (Comp "<>" (Real 5.0) (Real 5.0)) (Print (BExp True_C)) (Print (BExp False_C))) [] funcTable `shouldBe` ("False\n", [])
        parseStmt (IfElse (Comp "=" (Real 5.0) (Real 5.1)) (Print (BExp True_C)) (Print (BExp False_C))) [] funcTable `shouldBe` ("False\n", [])
        parseStmt (IfElse (Comp "<=" (Real 6.0) (Real 5.0)) (Print (BExp True_C)) (Print (BExp False_C))) [] funcTable `shouldBe` ("False\n", [])