module Task2
  ( Point
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , difference
  , perimeter
  , helper
  , doubleArea
  ) where

import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Array.MArray
import Data.Foldable
import Data.List

data Point = Point {
  x :: Int,
  y :: Int
} deriving Show

plus :: Point -> Point -> Point
plus a b = Point { x = x a + x b, y = y a + y b }

minus :: Point -> Point -> Point
minus a b = Point { x = x a - x b, y = y a - y b }

scalarProduct :: Point -> Point -> Int
scalarProduct a b = x a * x b + y a * y b

crossProduct :: Point -> Point -> Int
crossProduct a b = x a * y b - y a * x b

difference :: Point -> Point -> Double
difference a b = let t = minus a b in sqrt $ fromIntegral $ scalarProduct t t

helper :: Num b => [(Point, Point)] -> ((Point, Point) -> b) -> b -> b
helper arr f acc = runST $ do
  arr' <- newListArray (0, length arr - 1) arr :: ST s (STArray s Int (Point, Point))
  res <- newSTRef acc
  for_ [0..length arr - 1] $ \i -> do
    el <- readArray arr' i
    modifySTRef res (+ f el)
  modifySTRef res abs
  readSTRef res

perimeter  :: [Point] -> Double
perimeter [] = 0.0
perimeter q@(p:ps) = helper edges (uncurry difference) 0.0 where
  edges = zip q (ps++[p])

doubleArea :: [Point] -> Int
doubleArea q@(p0:p:ps) = helper points (uncurry crossProduct) 0 where
  points = map (\(a, b) -> (minus a p0, minus b p0)) points'
  points' = zip (p:ps) ps

t :: [Point]
t = map (\(a, b) -> Point {x = a, y = b} ) $ [(0,0), (0,1), (1,1), (1, 0)]
