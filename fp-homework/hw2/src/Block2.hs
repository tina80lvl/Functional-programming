module Block2
  ( eval
  ) where

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

-- task 1
data Expr = Const Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr

data ArithmeticError e = DivByZero e | NegativePow e

eval :: Expr -> Either ArithmeticError Int
eval (Const n) = Right n
eval (Add a b) = (+) <$> eval a <*> eval b
eval (Sub a b) = (-) <$> eval a <*> eval b
eval (Mul a b) = (*) <$> eval a <*> eval b
eval (Div a b) = eval b >>= \x -> if x == 0 then Left DivByZero 
  else fmap (`div` x) (eval a)
eval (Pow a b) = eval b >>= \x -> if x < 0 then Left NegativePow
  else fmap (^ x) (eval a)
-- TODO testing

-- task 2
-- TODO moving

-- task 3
-- class MonadFish m where
--     returnFish :: a -> m a
--     (>=>)      :: (a -> m b) -> (b -> m c) -> a -> m c
--
-- class MonadJoin m where
--     returnJoin :: a -> m a
--     join       :: m (m a) -> m a
--
-- class Monad m where
--     (>>=)  :: m a -> (a -> m b) -> m b
--     return :: a -> m a
--
-- instance MonadFish m => MonadJoin m
--   where
--     returnJoin = returnFish
--     join       = id >=> id
--
-- instance MonadFish m => Monad m
--   where
--     return     = returnFish
--     mx (>>=) f = id >=> f mx
--
-- instance Monad m => MonadFish m
--   where
--     returnFish  = return
--     f >=> g = \x -> f x >>= g
--
-- instance Monad m => MonadJoin m
--   where
--     returnJoin = return
--     join mmx   = mmx >>= id
