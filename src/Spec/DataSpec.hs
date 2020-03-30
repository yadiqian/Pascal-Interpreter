import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Pascal.Data

main :: IO ()
main = hspec $ do
  describe "toFloat" $ do
    it "Converts ValueT to float" $ do
      toFloat (Float 3.3) `shouldBe` 3.3
      toFloat (Float (-4.5)) `shouldBe` (-4.5)
    it "Errors on type mismatch" $ do
      evaluate (toFloat (Bool True)) `shouldThrow` errorCall "Value cannot be converted to float"
      evaluate (toFloat (Bool False)) `shouldThrow` errorCall "Value cannot be converted to float"

  describe "toBool" $ do
    it "Converts ValueT to float" $ do
      toBool (Bool True) `shouldBe` True
      toBool (Bool False) `shouldBe` False
    it "Errors on type mismatch" $ do
      evaluate (toBool (Float 3.3)) `shouldThrow` errorCall "Value cannot be converted to bool"
      evaluate (toBool (Float (-4.5))) `shouldThrow` errorCall "Value cannot be converted to bool"

  describe "toString" $ do
    it "Converts ValueT to String" $ do
      toString (Float 4.5) `shouldBe` "4.5"
      toString (Float (-3.0)) `shouldBe` "-3.0"
      toString (Bool True) `shouldBe` "True"

