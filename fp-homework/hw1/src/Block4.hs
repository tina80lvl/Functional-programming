module Block4
  ( splitOn
  , joinWith
  ) where

-- task 1
data Pair a = Pair a a
data NonEmpty a = a :| [a]
-- TODO

-- task 2
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c = foldr f [[]]
  where
    f a b = if a == c
            then [] : b
            else (a : head b) : tail b

joinWith :: a -> [[a]] -> [a]
joinWith n = foldMap (++ [n])
