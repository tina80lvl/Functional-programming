module Task2
  ( Point
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , perimeter
  , doubleArea
  ) where

import Number

type Point = Vector Number
data Line
    = PointSlope {
        point :: Point,
        slope :: Slope
    }
    | PointPoint {
        p1, p2 :: Point
    }
    deriving (Eq, Show, Read)

data Vector a
    = Vector a a
    deriving (Eq, Show, Read)

instance Num a => Num (Vector a) where
    (Vector x1 y1) + (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)
    (Vector x1 y1) - (Vector x2 y2) = Vector (x1 - x2) (y1 - y2)
    (Vector x1 y1) * (Vector x2 y2) =
        Vector (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)
    negate (Vector x y) =
        Vector (negate x) (negate y)
    abs (Vector x y) =
        Vector (abs x) (abs y)
    signum (Vector x y) =
        Vector (signum x) (signum y)
    fromInteger x =
        let int = fromInteger x
        in Vector int int

plus          :: Point -> Point -> Point

minus         :: Point -> Point -> Point

scalarProduct :: Point -> Point -> Int

crossProduct  :: Point -> Point -> Int

perimeter  :: [Point] -> Double -- Считает периметр

doubleArea :: [Point] -> Int    -- Считает удвоенную площадь
