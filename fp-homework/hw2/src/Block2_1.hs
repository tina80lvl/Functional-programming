module Block2_1
  ( eval ) where

-- task 1
data Expr = Const Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr

data ArithmeticError = DivByZero | NegativePow deriving (Eq, Show)

eval :: Expr -> Either ArithmeticError Int
eval (Const n) = Right n
eval (Add a b) = (+) <$> eval a <*> eval b
eval (Sub a b) = (-) <$> eval a <*> eval b
eval (Mul a b) = (*) <$> eval a <*> eval b
eval (Div a b) = eval b >>= \x -> if x == 0 then Left DivByZero
  else fmap (`div` x) (eval a)
eval (Pow a b) = eval b >>= \x -> if x < 0 then Left NegativePow
  else fmap (^ x) (eval a)

-- task 2
-- TODO moving
