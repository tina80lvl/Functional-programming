{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lang where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Except

import ExprReader
import ExprState

data ExecError = ExecError ExprError Integer

instance Show ExecError where
  show (ExecError er i) = show er ++ " in " ++ show i ++ " line"

data Lang = Mut String Expr
          | Upd String Expr
          deriving (Show)

exec :: ( MonadState           Environment m
        , MonadError ExprError             m
        )
     => Lang -> m ()
exec (Mut s e) = do
  m <- get
  ei <- eval m e
  case ei of
    Right r -> defineVar s r
    Left l  -> throwError l
exec (Upd s e) = do
  m <- get
  ei <- eval m e
  case ei of
    Right r -> updateVar s r
    Left l  -> throwError l

simulation :: ( MonadState           Environment m
              , MonadError ExprError             m
              )
           => [Lang] -> m ()
simulation (x:xs) = do
  exec x
  simulation xs
simulation []     = return ()


runSim :: (Monad m) => [Lang] -> m (Either ExprError Environment)
runSim l = runExceptT $ execStateT (runVarState $ simulation l) Map.empty
