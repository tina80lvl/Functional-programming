module Task3
  ( stringXor
  , magic
  , magic'
  , demagic
  , demagic'
  , triangular
  , rotator
  , resubstitute
  , resubstitute'
  , gauss
  , verifySolution
  ) where

import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.List
import Data.Foldable

stringXor :: ([Bool], Bool) -> ([Bool], Bool) -> ([Bool], Bool)
stringXor (as1, b1) (as2, b2) = runST $ do
  let cs = zip as1 as2
  cs' <- newListArray (0, length cs - 1) cs :: ST s (STArray s Int (Bool, Bool))
  res <- newArray (0, length cs - 1) False :: ST s (STArray s Int Bool)
  for_ [0..length cs - 1] $ \i -> do
    (a, b) <- readArray cs' i
    writeArray res i (a /= b)
  val <- getElems res
  pure (val, b1 /= b2)

magic :: ([[a]], [a]) -> [[a]]
-- It's OK to use without threading cuz there will be no perfomance gain
magic (xs, x) = zipWith (++) xs (map (\e -> [e]) x)

magic' :: ([a], a) -> [a]
magic' (xs, x) = xs ++ [x]

demagic :: [[a]] -> ([[a]],[a])
demagic xs = (m, c) where
  c = map last xs
  m = map init xs

demagic' :: [a] -> ([a], a)
demagic' xs = (init xs, last xs)

triangular :: [[Bool]] -> Maybe [[Bool]]
triangular [] = Just []
triangular m  =
  let
    m' = rotator m $ replicate (length m) False
  in case m' of
      Nothing -> Nothing
      Just [] -> Nothing
      Just (row:rows) -> case let
        rows' = filter (any id) $ map f rows
        f bs
           | (not . head $ bs) = drop 1 bs
           | otherwise         = drop 1 $ magic' (stringXor (demagic' bs) (demagic' row))
        newm' = triangular rows' in newm' of
          Nothing -> Nothing
          Just val -> Just (row: val)

rotator :: [[Bool]] -> [Bool] -> Maybe [[Bool]]
rotator [] _ = Just []
rotator _ [] = Just []
rotator (row:rows) (v:vs) =
  if v
  then Nothing
  else
    if head row
    then Just (row:rows)
    else rotator (rows ++ [row]) (vs ++ [True])

resubstitute :: Maybe [[Bool]] -> Maybe [Bool]
resubstitute m =
  case fmap (find ((<= 1) . length)) m of
    Nothing -> Nothing
    Just Nothing -> fmap (reverse . resubstitute' . reverse . map reverse) m
    Just _ -> Nothing

resubstitute' :: [[Bool]] -> [Bool]
resubstitute' [] = []
resubstitute' (row:rows) = x:(resubstitute' rows')
  where
   x :: Bool
   x     = (head row)
   rows' :: [[Bool]]
   rows' = map substituteUnknown rows
   substituteUnknown :: [Bool] -> [Bool]
   substituteUnknown (a1:(a2:as')) = ((a1 /= (x && a2)):as')
   substituteUnknown _ = []

---------------------------------- TESTING -------------------------------------
-- t :: [[Bool]]
-- t = [[True,False, False],[False,True,False]]
--
-- c :: [Bool]
-- c = [True, False]
--------------------------------------------------------------------------------

gauss :: [[Bool]] -> [Bool] -> Maybe [Bool]
gauss ass cs = resubstitute . triangular $ magic (ass, cs)

verifySolution :: [[Bool]] -> [Bool] -> Bool
verifySolution m v = let (ass, c) = demagic m in
  runST $ do
    s <- newSTRef True
    c' <- newListArray (0, length c - 1) c :: ST s (STArray s Int Bool)
    vs' <- newListArray (0, length v - 1) v :: ST s (STArray s Int Bool)
    ass' <- newListArray (0, length ass - 1) ass :: ST s (STArray s Int [Bool])
    for_ (0::Int, length ass - 1) $ \j -> do
      rawRow <- readArray ass' j
      row' <- newListArray (0, length ass - 1) rawRow :: ST s (STArray s Int Bool)
      res <- newSTRef False
      for_ (0::Int, length v - 1) $ \i -> do
        el <- readArray row' i
        v' <- readArray vs' i
        modifySTRef res (/= (el && v'))
      val <- readSTRef res
      expected <- readArray c' j
      if val == expected then modifySTRef s id else modifySTRef s (const False)
    s' <- readSTRef s
    pure s'
