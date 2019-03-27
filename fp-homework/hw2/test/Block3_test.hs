module Block3_test
  ( spec
  , hspecTestTree
  ) where

import Block3
import Test.Tasty.Hspec
import Test.Tasty (TestTree)
import Data.Maybe (isJust, isNothing)

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "--- Testing Block 3 ---" spec

spec :: Spec
spec = do
    it "brackets" $ do
      runParser parseBrackets "())" `shouldSatisfy` (isNothing)
      runParser parseBrackets " ( ) ( ) " `shouldSatisfy` (isJust)
      runParser parseBrackets "((()()))" `shouldSatisfy` (isJust)
      runParser parseBrackets ")(" `shouldSatisfy` (isNothing)

    it "numbers" $ do
      runParser parseNumbers "1703" `shouldBe` Just (1703, "")
      runParser parseNumbers "+100500" `shouldBe` Just (100500, "")
      runParser parseNumbers " -375 " `shouldBe` Just (-375, "")
      runParser parseNumbers "+-9" `shouldSatisfy` (isNothing)

    it "arrays" $ do
      runParser parseArrays "2, 1,+10  , 3,5,-7, 2" `shouldBe` Just ([[1, 10], [5, -7, 2]], "")
      runParser parseArrays "1, 1, 4 ,33" `shouldSatisfy` (isNothing)
      runParser parseArrays " 1703 " `shouldSatisfy` (isNothing)
      runParser parseArrays " 0, 0 " `shouldBe` Just([[], []], "")
      runParser parseArrays "+2 , -4, -8" `shouldBe` Just ([[-4, -8]])
