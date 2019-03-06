module Block3
  ( nextDay
  , afterDays
  , isWeekend
  , daysToParty
  , natToInteger
  , isEmpty
  , size
  , findElement
  , insertElement
  , fromList
  ) where

-- task 1
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Ord, Enum, Show, Read)
-- TODO write instances

nextDay :: Day -> Day
nextDay Sun = Mon
nextDay d   = succ d

afterDays :: Day -> Int -> Day
afterDays d 0 = d
afterDays d n = afterDays (nextDay d) (rem (n - 1) 7)

isWeekend :: Day -> Bool
isWeekend d = (> Fri) d

daysToParty :: Day -> Int
daysToParty Fri = 0
daysToParty d   = daysToParty (nextDay d) + 1

-- task 2
-- TODO

-- task 3
data Nat = Z | S Nat deriving Show

natToInteger :: Nat -> Integer
natToInteger (S n) = natToInteger (n + 1)
natToInteger Z     = 0

instance Eq Nat
  where
    (S a) == (S b) = a == b
    Z == Z = True
    _ == _ = False

instance Ord Nat
  where
    compare (S a) (S b) = compare a b
    compare (S _) Z     = GT
    compare Z (S _)     = LT
    compare Z Z         = EQ

instance Num Nat
  where
    (S a) + (S b) = S (S (a + b))
    (S n) + Z = S n
    Z + (S n) = S n
    Z + Z = Z

    (S a) * b = (a * b) + b
    Z * _ = Z

    (S a) - (S b) = a - b
    Z - _ = Z
    n - Z = n

    abs a = a

    signum (S _) = S Z
    signum Z     = Z

    fromInteger z
      | z <= 0    = Z
      | otherwise = S (fromInteger (z - 1))

-- task 4
data Tree a = Leaf | Node a (Tree a) (Tree a)

isEmpty :: Ord a => Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

size :: Ord a => Tree a -> Integer
size Leaf             = 0
size (Node _ ch1 ch2) = size ch1 + size ch2 + 1

findElement :: Ord a => Tree a -> a -> Bool
findElement Leaf _ = False
findElement (Node e ch1 ch2) n = case compare e n of
                               EQ -> True
                               GT -> findElement ch1 n
                               LT -> findElement ch2 n

insertElement :: Ord a => Tree a -> a -> Tree a
insertElement Leaf a = Node a Leaf Leaf
insertElement n@(Node e ch1 ch2) a = case compare e a of
                                    EQ -> n
                                    GT -> Node e (insertElement ch1 a) ch2
                                    LT -> Node e ch1 (insertElement ch2 a)

fromList :: Ord a => [a] -> Tree a
fromList (h:tl) = insertElement (fromList tl) h
fromList []     = Leaf

-- TODO remove element
