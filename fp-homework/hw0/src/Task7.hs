module Task7
  (
  ) where

-- infixr 0 $
-- infixr 9 .
-- (null . head) $ (map (uncurry id) [((++) "Dorian ", " Grey")])
-- " Grey" :: [Char]
-- (++) "Dorian " :: [Char] -> [Char]
-- [((++) "Dorian ", " Grey")] :: [([Char] -> [Char], [Char])]
-- (uncurry id) :: (b -> c, b) -> c
-- map (uncurry id) [((++) "Dorian ", " Grey")] :: [[Char]]
-- (null . head) :: Foldable t => [t a] -> Bool
-- (null . head) $ (map (uncurry id) [((++) "Dorian ", " Grey")]) :: Bool

-- (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
-- (Num a, Num b) => [(a, b)]

-- let impl = \x y -> not x || y in
--     let isMod2 = \x -> x `mod` 2 == 0 in
--     let isMod4 = \x -> x `mod` 4 == 0 in
--     \x -> (isMod4 x) `impl` (isMod2 x)
