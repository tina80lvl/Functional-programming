module Block1
  ( order3
  , smartReplicate
  , contains
  , stringSum
  ) where

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (min a (min b c), max (min a c) (min b c), max a (max b c))

smartReplicate :: [Int] -> [Int]
smartReplicate [] = []
smartReplicate (h:tl) = replicate h h ++ (smartReplicate tl)

contains :: Eq a => a -> [[a]] -> [[a]]
contains a = filter (elem a)

stringSum :: String -> Int
stringSum s = sum (map read (words s)) --TODO
