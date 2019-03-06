{-# LANGUAGE TypeOperators #-}
module Task1
  ( distributivity
  , associator
  , eitherAssoc
  ) where

distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a) = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)
eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (leftF, rightF)
  where
    -- for ->
    leftF (Left a) = (Left (Left a))
    leftF (Right (Left b)) = (Left (Right b))
    leftF (Right (Right c)) = (Right c)
    -- for <-
    rightF (Left (Left a)) = (Left a)
    rightF (Left (Right b)) = (Right (Left b))
    rightF (Right c) = (Right (Right c))
