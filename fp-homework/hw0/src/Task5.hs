module Task5
  ( zero
  , succChurch
  , churchPlus
  -- , churchMult
  -- , churchToInt
  ) where

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero f x = x

succChurch :: Nat a -> Nat a
succChurch n f x = f (n f x)

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus a b f x = a f (b f x)

-- churchMult :: Nat a -> Nat a -> Nat a
-- churchMult a b = a (churchPlus b) zero

-- churchToInt :: Nat Integer -> Integer
-- churchToInt = undefined
