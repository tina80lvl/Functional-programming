module Parser
  ( sc
  , lexeme
  , symbol
  , parens
  , integer
  , rword
  , rws
  , identifier
  , langParser
  , assignment
  , mutVar
  , updVar
  , expr
  , letExpr
  , getExpr
  , exprOperators
  , exprTerm
  )  where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as Lexer

import ExprReader
import Lang

type Parser = Parsec Void String

sc :: Parser ()
sc = Lexer.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = Lexer.skipLineComment "//"
    blockCmnt = Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: String -> Parser String
symbol = Lexer.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme Lexer.decimal

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["let", "in", "mut"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

langParser :: Parser Lang
langParser = between sc eof assignment

assignment :: Parser Lang
assignment = mutVar <|> updVar

mutVar :: Parser Lang
mutVar = do
  rword "mut"
  var  <- identifier
  void (symbol "=")
  val  <- expr
  return $ Mut var val

updVar :: Parser Lang
updVar = do
  var  <- identifier
  void (symbol "=")
  val  <- expr
  return $ Upd var val

expr :: Parser Expr
expr = letExpr <|> getExpr

letExpr :: Parser Expr
letExpr = do
  rword "let"
  var  <- identifier
  void (symbol "=")
  val  <- getExpr
  rword "in"
  inExpr  <- getExpr
  return $ Let var val inExpr

getExpr :: Parser Expr
getExpr = makeExprParser exprTerm exprOperators

exprOperators :: [[Operator Parser Expr]]
exprOperators =
  [ [ Prefix (Neg <$ symbol "-") ]
  , [ InfixL (Mul <$ symbol "*")
    , InfixL (Div <$ symbol "/") ]
  , [ InfixL (Add <$ symbol "+")
    , InfixL (Sub <$ symbol "-") ]
  ]

exprTerm :: Parser Expr
exprTerm = parens expr
  <|> Var <$> identifier
  <|> Lit <$> integer

-- parserCreateOrAssign :: (String -> Expr -> Action) -> Parser Action
-- parserCreateOrAssign constrAction = do
--     varName <- identifier
--     symbol "="
--     expr <- parserExpr
--     return $ constrAction varName expr
--
-- parserCreate :: Parser Action
-- parserCreate = do
--     keyword "mut"
--     parserCreateOrAssign Create
--
-- parserAssign :: Parser Action
-- parserAssign = parserCreateOrAssign Assign
--
-- parserRead :: Parser Action
-- parserRead = do
--     symbol ">"
--     varName <- identifier
--     return $ Read varName
--
-- parserWrite :: Parser Action
-- parserWrite = do
--     symbol "<"
--     expr <- parserExpr
--     return $ Write expr
--
-- parserFor :: Parser Action
-- parserFor = do
--     keyword "for"
--     symbol  "("
--     varName <- identifier
--     keyword "from"
--     fromExpr <- parserExpr
--     keyword "to"
--     toExpr <- parserExpr
--     symbol  ")"
--     symbol  "{"
--     actions <- parserProgram
--     symbol  "}"
--     return $ For varName fromExpr toExpr actions (getActionsLength actions)
--   where
--     getActionsLength :: [Action] -> Int
--     getActionsLength actions' = sum $ map getActionLength actions'
--
--     getActionLength :: Action -> Int
--     getActionLength (For _ _ _ _ len) = 2 + len
--     getActionLength _                 = 1
--
-- parserBreak :: Parser Action
-- parserBreak = do
--     keyword "break"
--     return Break
--
-- parserAction :: Parser Action
-- parserAction =
--     parserCreate
--     <|> parserAssign
--     <|> parserRead
--     <|> parserWrite
--     <|> parserFor
--     <|> parserBreak
--
-- parserProgram :: Parser [Action]
-- parserProgram = many parserAction
