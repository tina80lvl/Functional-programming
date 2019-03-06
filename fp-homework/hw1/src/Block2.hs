module Block2
  ( randomIntList
  , removeNth
  , mergeSort
  ) where

import System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

removeNth :: Int -> [a] -> [a]
removeNth _ [] = []
removeNth i (h:tl)
  | i > 0     = h : removeNth (i - 1) tl
  | i == 0    = tl
  | otherwise = h : tl

mergeSort :: Ord a => [a] -> [a]
mergeSort [x] = [x]
mergeSort xs = let (a, b) = splitAt (div (length xs) 2) xs in merge (mergeSort a) (mergeSort b)
  where
    merge :: Ord a => [a] -> [a] -> [a]
    merge [] b = b
    merge a [] = a
    merge (a:as) (b:bs) = if a < b then a : merge as (b : bs) else b : merge (a : as) bs
