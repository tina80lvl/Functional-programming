{-# LANGUAGE TypeOperators #-}
import Data.Void (Void)
import Data.Maybe (mapMaybe)
import Data.Function (fix)

-- task 1
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

-- task 2
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

-- task 3
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

composition :: (b -> c) -> (a -> b) -> a -> c
composition f g x = s (const f) g x

identity :: a -> a
identity x = flip const (const (const s)) x

contraction :: (a -> a -> b) -> a -> b
-- Same as (f x x)
contraction f x = flip const (s (flip const f)) x
(a -> b -> c) -> ((a -> b) -> (a -> c))

-- permutation :: (a -> b -> c) -> b -> a -> c
-- permutation = undefined

-- task 4
iterateElement :: a -> [a]
iterateElement a = fix (a :)

-- fibonacci :: Integer -> Integer --TODO
-- fibonacci a = fix (\rec a -> if a <= 1 then 1 else a + rec (a - 1)) a

factorial :: Integer -> Integer
factorial a = fix (\rec a -> if a <= 1 then 1 else a * rec (a - 1)) a

-- mapFix :: (a -> b) -> [a] -> [b]
-- mapFix = undefined

-- -- task 5
-- type Nat a = (a -> a) -> a -> a

-- zero :: Nat a
-- zero f x = x

-- succChurch :: Nat a -> Nat a
-- succChurch = undefined

-- churchPlus, churchMult
--     :: Nat a -> Nat a -> Nat a
-- churchPlus = undefined
-- churchMult = undefined
--
-- churchToInt :: Nat Integer -> Integer
-- churchToInt = undefined
--
-- -- task 6
-- -- distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))
--
--
-- -- null $ mapMaybe foo "pole chudes ochen' chudesno"
--
-- foo :: Char -> Maybe Double
-- foo char =
--     case char == 'o' of
--       True -> Just $ exp pi
--       False -> Nothing
--
-- -- task 7
-- -- null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]
-- -- (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
--
-- -- let impl = \x y -> not x || y in
-- --     let isMod2 = \x -> x `mod` 2 == 0 in
-- --     let isMod4 = \x -> x `mod` 4 == 0 in
-- --     \x -> (isMod4 x) `impl` (isMod2 x)
