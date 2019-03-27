{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Block2_2 where

import Prelude hiding (Monad, (>=>), (>>=), return)

-- task 3
class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> a -> m c

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    return :: a -> m a


instance MonadFish m => MonadJoin m
  where
    returnJoin = returnFish
    join       = id >=> id

instance Monad m => MonadFish m
  where
    returnFish  = return
    f >=> g = \x -> f x >>= g
