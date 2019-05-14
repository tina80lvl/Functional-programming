{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ExprReader
  ( Expr (..)
  , ExprError (..)
  , Environment
  , eval
  )
  where

import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Control.Monad.Except

data Expr
          = Lit Integer
          | Neg Expr
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let String Expr Expr
          deriving (Show)

data Action
          = Create String Expr
          | Assign String Expr
          | Read String
          | Write Expr
          | For String Expr Expr [Action] Int
          | Break
          deriving (Show)

data ExprError = VariableNotFound String
               | VariableAlreadyDefined String
               | DivideByZero
               deriving (Show)

type Environment = Map.Map String Integer

newtype ExprReader m a = ExprReader { runExprReader :: ReaderT Environment (ExceptT ExprError m) a }
      deriving (Functor, Applicative, Monad, MonadError ExprError, MonadReader Environment)

getExprReader :: ( MonadReader Environment           m
                 , MonadError              ExprError m
                 )
              => Expr -> m Integer
getExprReader (Lit n) = return n
getExprReader (Neg e) = negate <$> getExprReader e
getExprReader (Add a b) = (+) <$> getExprReader a <*> getExprReader b
getExprReader (Sub a b) = (-) <$> getExprReader a <*> getExprReader b
getExprReader (Mul a b) = (*) <$> getExprReader a <*> getExprReader b
getExprReader (Div a b) = do
  divider <- getExprReader b
  if divider == 0
    then throwError DivideByZero
    else (`div` divider) <$> getExprReader a
getExprReader (Var n) = do
  v <- asks $ Map.lookup n
  case v of
    Nothing -> throwError $ VariableNotFound n
    Just j  -> return j
getExprReader (Let s a b) = do
  val <- getExprReader a
  local (Map.insert s val) (getExprReader b)

eval :: Monad m => Environment -> Expr -> m (Either ExprError Integer)
eval en ex = runExceptT $ runReaderT (runExprReader $ getExprReader ex) en
