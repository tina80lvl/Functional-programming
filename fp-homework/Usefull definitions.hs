($) :: (a -> b) -> a -> b
(.) :: (b -> c) -> (a -> b) -> a -> c

fix :: (a -> a) -> a
fix f = let x = f x in x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x
