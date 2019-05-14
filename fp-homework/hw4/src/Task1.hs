module Task1
  ( parHelp
  , helpMult
  , multiply
  ) where

import Data.Vector
import Data.List
import Control.Parallel
import Control.Concurrent

parHelp :: [Int] -> [Int] -> Int
-- parHelp :: ( Num a ) => [ a ] -> [ a ] -> a
parHelp [] [] = 0
parHelp ( x : xs ) ( y : ys ) = ret where
ret = par a ( pseq b ( a + b ) ) where
        a = x * y
        b = parHelp xs ys

helpMult :: [Int] -> [[Int]] -> [Int]
-- helpMult :: ( Num a ) => [ a ] -> [ [ a ] ] -> [ a ]
helpMult _ [] = []
helpMult x ( y : ys ) = ret where
 ret =  par a ( pseq b  ( a : b ) ) where
   a = sum . zipWith ( *) x $ y
   b = helpMult x ys

-- TODO add Maybe
multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
-- mult :: ( Num a ) => [ [ a ] ] -> [ [ a ] ] -> [ [ a ] ]
mult [] _ = []
mult ( x : xs ) ys = ret where
 ret = par a ( pseq b  ( a : b ) ) where
    a = helpMult x ys
    b = mult xs ys
