{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ExprState
  ( defineVar
  , updateVar
  , runVarState
  ) where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Except

import ExprReader

newtype VarState m a = VarState { runVarState :: StateT Environment (ExceptT ExprError m) a }
  deriving (Functor, Applicative, Monad, MonadError ExprError, MonadState Environment)

defineVar :: ( MonadState          Environment m
             , MonadError ExprError             m
             )
          => String -> Integer -> m ()
defineVar s n = do
  exist <- gets $ Map.member s
  if exist
    then throwError $ VariableAlreadyDefined s
    else modify $ Map.insert s n

updateVar :: ( MonadState          Environment m
             , MonadError ExprError             m
             )
          => String -> Integer -> m ()
updateVar s n = do
  exist <- gets $ Map.member s
  if exist
    then modify $ Map.insert s n
    else throwError $ VariableNotFound s

-- def :: (Monad m) => String -> Integer -> m (Either EnvError Environment)
-- def s n = runExceptT $ execStateT (runVarState $ defineVar s n) Map.empty
--
-- upd :: (Monad m) => String -> Integer -> m (Either EnvError Environment)
-- upd s n = runExceptT $ execStateT (runVarState $ updateVar s n) Map.empty

-- env :: Environment
-- env = Map.insert "x" 1 Map.empty
