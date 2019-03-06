module Task4
  ( iterateElement
  , fibonacci
  , factorial
  , mapFix
  ) where

import Data.Function (fix)

iterateElement :: a -> [a]
iterateElement a = fix (a :)

fibonacci :: Integer -> Integer
fibonacci a = fix (\rec x -> if (x == 1 || x == 2) then 1
  else (rec (x - 1) + rec (x - 2))) a

factorial :: Integer -> Integer
factorial a = fix (\rec x -> if x <= 1 then 1 else x * rec (x - 1)) a

mapFix :: (a -> b) -> [a] -> [b]
mapFix f a = fix (\rec x -> if null x then [] else (f (head x) : rec (tail x))) a
