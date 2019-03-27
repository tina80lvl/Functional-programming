module Block1
  ( stringSum
  ) where

import Text.Read

-- task 1
stringSum :: String -> Maybe Int
stringSum s = sum <$> (sequenceA (map readMaybe (words s)))

-- task 2
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch l r) = (Branch (fmap f l) (fmap f r))

instance Applicative Tree where
  pure x = Leaf x
  (<*>) (Leaf f) x = fmap f x
  (<*>) f (Leaf x) = (Leaf (\g -> g x)) <*> f
  (<*>) (Branch l1 r1) (Branch l2 r2) = Branch (l1 <*> l2) (r1 <*> r2)

instance Foldable Tree where
  foldr f z (Leaf x) = f x z
  foldr f z (Branch l r) = foldr f (foldr f z r) l

instance Traversable Tree where
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Branch l r) = Branch <$> traverse f l <*> traverse f r

-- task 3
data NonEmpty a = a :| [a]

instance Functor NonEmpty where
  fmap f (a :| as) = (f a) :| (fmap f as)

instance Applicative NonEmpty where
  pure a = a :| []
  (<*>) (f :| fs) (a :| as) = (f a) :| rest where
    rest = concat [(fmap f as), (fmap (\g -> g a) fs), (fs <*> as)]

instance Foldable NonEmpty where
  foldr f z (a :| as) = f a (foldr f z as)

instance Traversable NonEmpty where
  traverse f = sequenceA . fmap f
