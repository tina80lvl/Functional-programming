module Block3
  ( first
  ) where

import Data.Char (isDigit, isSpace, isUpper, isAlpha, isAlphaNum)
import Control.Monad (void, (>=>))
import Control.Applicative

-- newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser
  where
    fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser
  where
    pure a    = Parser (\s -> return (a, s))
    p1 <*> p2 = Parser (runParser p1 >=> (\ (f, rs) -> runParser (fmap f p2) rs))

-- instance Monad Parser where
--   return = pure
--   (Parser p) >>= qf = Parser $ \s -> do
--     (r, t) <- p s
--     runP (qf r) t

instance Alternative Parser
  where
    empty     = Parser (const Nothing)
    p1 <|> p2 = Parser (\s -> runParser p1 s <|> runParser p2 s)

-- task 2
-- TODO ok
-- TODO eof
-- satisfy :: (Char -> Bool) -> Parser Char
-- satisfy p = Parser f
--   where
--     f [] = Nothing
--     f (x:xs)
--       | p x       = Just (x, xs)
--       | otherwise = Nothing
-- TODO element
-- TODO stream

-- task 3


-- task 4
