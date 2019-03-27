{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Block2_3 where

import Prelude hiding (Monad, (>=>), (>>=), return)

-- task 3 (continue)
class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> a -> m c

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    return :: a -> m a


instance MonadFish m => Monad m
  where
    return  = returnFish
    m >>= f = (const m >=> f) undefined

instance Monad m => MonadJoin m
  where
    returnJoin = return
    join mmx   = mmx >>= id
