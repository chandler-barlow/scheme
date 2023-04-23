{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal (Eval, EnvCtx, LispVal) where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Text as T

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval {runEval :: ReaderT EnvCtx IO a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader EnvCtx,
      MonadIO
    )

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool
  deriving (Eq)
