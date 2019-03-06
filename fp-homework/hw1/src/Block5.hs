{-# LANGUAGE InstanceSigs #-}
module Block5
  ( maybeConcat
  , fromString
  , toString
  ) where

import Data.Semigroup (Semigroup ((<>)))

-- task 1
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldMap f
  where
    f :: Maybe [a] -> [a]
    f (Just x) = x
    f Nothing  = []

-- task 2
data NonEmpty a = a :| [a]

instance Semigroup (NonEmpty a)
  where
    (a :| as) <> (b :| bs) = a :| (as <> (b : bs))

-- task 3
data Builder = One Char | Many [Builder] deriving (Show)

instance Semigroup Builder
  where
    (<>) :: Builder -> Builder -> Builder
    Many lb1 <> Many lb2 = Many (lb1 ++ lb2)
    b1 <> Many lb2       = Many (b1 : lb2)
    Many lb1 <> b2       = Many (lb1 ++ [b2])
    b1 <> b2             = Many [b1, b2]

instance Monoid Builder
  where
    mempty :: Builder
    mempty = Many []

    mappend :: Builder -> Builder -> Builder
    mappend b1 b2 = b1 <> b2

fromString :: String -> Builder
fromString s = Many (map One s)

toString :: Builder -> String
toString (One c)   = [c]
toString (Many lb) = concatMap getString lb
  where
    getString (One x) = [x]
    getString b       = toString b
