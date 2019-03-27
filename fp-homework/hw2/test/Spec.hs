module Main where

import Test.Tasty (defaultMain, testGroup)

import Block1_test (hspecTestTree)

main :: IO ()
main = hspecTestTree >>= \unitTests ->
       let allTests = testGroup "Parser" [unitTests]
       in defaultMain allTests
