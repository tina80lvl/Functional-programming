module Task2
  ( doubleNeg
  , excludedNeg
  , pierce
  , doubleNegElim
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
-- a -> (a -> Void) -> Void
doubleNeg a f = f a

excludedNeg :: Neg (Neg (Either a (Neg a)))
-- ((Either a (a -> Void)) -> Void) -> Void
-- Right :: (a -> Void) -> Either a (a -> Void)
-- Left :: a -> Either a (a -> Void)
-- (x . Right) :: (a -> Void) -> Void
-- Analogically to (x . Left)
excludedNeg x = (x . Right) (x . Left)

pierce :: ((a -> b) -> a) -> a
pierce = undefined -- NO WAY to derive type: model Kripke

doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined -- NO WAY to derive type: 10th axiom

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
-- (((a -> Void) -> Void) -> Void) -> a -> Void
thirdNegElim x c = x ($ c)
