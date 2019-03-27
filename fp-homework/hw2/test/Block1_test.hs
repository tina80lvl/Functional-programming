module Block1_test
  ( spec
  , hspecTestTree
  ) where

import Block1
import Test.Tasty.Hspec
import Test.Tasty (TestTree)

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "--- Testing Block 1 ---" spec

spec :: Spec
spec = do
    it "ok" $ do
      stringSum "1 2" `shouldBe` Just 3
      stringSum "3 -5" `shouldBe` Just (-2)
      stringSum " 1 7 0 3 " `shouldBe` Just 11
      stringSum "100 -500 600" `shouldBe` Just 200

    it "err" $ do
      stringSum "3 pi14" `shouldBe` Nothing
      stringSum "behappy" `shouldBe` Nothing
