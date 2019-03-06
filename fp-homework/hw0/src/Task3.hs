module Task3
  ( s
  , composition
  , identity
  , contraction
  , permutation
  ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

composition :: (b -> c) -> (a -> b) -> a -> c
composition f g x = s (const f) g x

identity :: a -> a
identity x = const x (const (const s))

contraction :: (a -> a -> b) -> a -> b
-- Same as (f x x)
contraction f = s f identity

permutation :: (a -> b -> c) -> b -> a -> c
permutation f x y = s f (const x) y
