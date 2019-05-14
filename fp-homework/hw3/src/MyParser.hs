module MyParser
  ( eval
  , first
  , ok
  , eof
  , satisfy
  , element
  , stream
  , parseBrackets
  , toNumber
  , parseNumbers
  , parseNumber
  , listParse
  , parseArrays
  ) where
----------------------------------- FROM HW2 -----------------------------------
import Data.Char (isDigit, digitToInt)
import Control.Monad ((>=>))
import Control.Applicative

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

-- task 1
data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor (Parser s)
  where
    fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative (Parser s)
  where
    pure a    = Parser (\s -> return (a, s))
    p1 <*> p2 = Parser (runParser p1 >=> (\ (f, rs) -> runParser (fmap f p2) rs))

instance Monad (Parser s)
  where
    return = pure
    (Parser p) >>= qf = Parser $ \s -> do
      (r, t) <- p s
      runParser (qf r) t

instance Alternative (Parser s)
  where
    empty     = Parser (const Nothing)
    p1 <|> p2 = Parser (\s -> runParser p1 s <|> runParser p2 s)

-- task 2
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof = Parser $ \s -> case s of
    [] -> Just ((), [])
    _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing

element :: Eq s => s -> Parser s s
element el = satisfy (== el)

stream :: Eq s => [s] -> Parser s [s]
stream = foldr ((<*>) . fmap (:) . element) ([] <$ ok)

-- task 3
parseBrackets :: Parser Char ()
parseBrackets = brackets *> eof
  where
    brackets = (element '(' *> brackets *> element ')' *> brackets) <|> ok

toNumber :: Parser Char Int
toNumber = fmap digitToInt (satisfy isDigit)

parseNumbers :: Parser Char Int
parseNumbers = symb <*> (foldl (\f a -> f * 10 + a) 0 <$> some toNumber)
  where
    symb = id <$ element '+' <|> negate <$ element '-' <|> id <$ ok

-- task 4
parseNumber :: Int -> Parser Char [Int]
parseNumber 1 = fmap return parseNumbers
parseNumber i = fmap (:) (parseNumbers <* element ',') <*> parseNumber (i - 1)

listParse :: Parser Char [Int]
listParse = (>>=) (parseNumbers <* element ',') parseNumber

parseArrays :: Parser Char [[Int]]
parseArrays = ((:) <$> listParse <*> many (element ',' *> listParse))
  <|> const [] <$> ok

----------------------------------- NEW PART -----------------------------------
parseString ::
