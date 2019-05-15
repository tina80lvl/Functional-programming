module Task1
  ( multVV
  , multVM
  , multiply
  ) where

import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.List
import Data.Foldable
import Data.Array.MArray

multVV :: [Int] -> [Int] -> Maybe Int
multVV a b =
  if length a == length b
  then
    runST $ do
      a' <- newListArray (0, length a - 1) a :: ST s (STUArray s Int Int)
      b' <- newListArray (0, length b - 1) b :: ST s (STUArray s Int Int)
      res <- newSTRef 0
      for_ [0..length a - 1] $ \i -> do
        l <- readArray a' i
        r <- readArray b' i
        modifySTRef res (+ l * r)
      val <- readSTRef res
      pure $ Just val
  else Nothing

multVM :: [Int] -> [[Int]] -> Maybe [Int]
multVM v m =
  if length v == (length . head $ m)
  then
    runST $ do
      m' <- newListArray (0, length m - 1) m :: ST s (STArray s Int [Int])
      res <- newArray (0, length m - 1) Nothing :: ST s (STArray s Int (Maybe Int))
      for_ [0..length m - 1] $ \i -> do
        s <- readArray m' i
        writeArray res i $ multVV v s
      val <- getElems res
      pure $ sequence val
  else Nothing

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply [] [] = Just []
multiply [] _ = Nothing
multiply _ [] = Nothing
multiply xs ys =
  let yst = transpose ys in
    runST $ do
      xs' <- newListArray (0, length xs - 1) xs :: ST s (STArray s Int [Int])
      res <- newArray (0, length xs - 1) Nothing :: ST s (STArray s Int (Maybe [Int]))
      for_ [0..length xs - 1] $ \i -> do
        s <- readArray xs' i
        writeArray res i $ multVM s yst
      val <- getElems res
      pure $ sequence val
