module Block2_test
  ( spec
  , hspecTestTree
  ) where

import Block2_1
import Block2_2
import Block2_3
import Test.Tasty.Hspec
import Test.Tasty (TestTree)

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "--- Testing Block 2 ---" spec

spec :: Spec
spec = do
    it "ok" $ do
      eval (Add (Const 2) (Const 5)) `shouldBe` Right 7
      eval (Sub (Const 5) (Const 2)) `shouldBe` Right 3
      eval (Mul (Const 2) (Const 5)) `shouldBe` Right 10
      eval (Div (Const 8) (Const 2)) `shouldBe` Right 4
      eval (Pow (Const 2) (Const 4)) `shouldBe` Right 16

    it "err" $ do
      eval (Div (Const 2) (Const 0)) `shouldBe` Left DivByZero
      eval (Pow (Const 2) (Const (-6))) `shouldBe` Left NegativePow
