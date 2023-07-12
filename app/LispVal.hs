{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module LispVal
  ( Eval (..),
    EnvCtx (..),
    LispVal (..),
    IFunc (..),
    showVal,
    LispException (..),
  )
where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Text as T

type EnvCtx = M.Map T.Text LispVal

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

data IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

instance Eq IFunc where
  (==) _ _ = False

data LispException
  = NumArgs Integer [LispVal]
  | LengthOfList T.Text Int
  | ExpectedList T.Text
  | TypeMismatch T.Text LispVal
  | Parser T.Text
  | BadSpecialForm T.Text
  | NotFunction LispVal
  | UnboundVar T.Text
  | Default LispVal
  | PError String
  | IOError T.Text

instance Exception LispException

instance Show LispException where
  show = T.unpack . showError

showError :: LispException -> T.Text
showError err =
  case err of
    (IOError txt) ->
      T.concat
        [ "Error reading file: ",
          txt
        ]
    (NumArgs int args) ->
      T.concat
        [ "Error Number Arguments, expected ",
          T.pack $ show int,
          " recieved args: ",
          unwordsList args
        ]
    (LengthOfList txt int) ->
      T.concat
        [ "Error Length of List in ",
          txt,
          " length: ",
          T.pack $ show int
        ]
    (ExpectedList txt) ->
      T.concat
        [ "Error Expected List in funciton ",
          txt
        ]
    (TypeMismatch txt val) ->
      T.concat
        [ "Error Type Mismatch: ",
          txt,
          showVal val
        ]
    (BadSpecialForm txt) ->
      T.concat
        [ "Error Bad Special Form: ",
          txt
        ]
    (NotFunction val) ->
      T.concat
        [ "Error Not a Function: ",
          showVal val
        ]
    (UnboundVar txt) ->
      T.concat
        [ "Error Unbound Variable: ",
          txt
        ]
    (PError str) ->
      T.concat
        [ "Parser Error, expression cannot evaluate: ",
          T.pack str
        ]
    (Default val) ->
      T.concat
        [ "Error, Danger Will Robinson! Evaluation could not proceed!  ",
          showVal val
        ]

unwordsList :: [LispVal] -> T.Text
unwordsList vals = T.unwords $ map showVal vals

instance Show LispVal where
  show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal val =
  case val of
    (Atom atom) -> atom
    (String str) -> T.concat ["\"", str, "\""]
    (Number num) -> T.pack $ show num
    (Bool True) -> "#t"
    (Bool False) -> "#f"
    Nil -> "Nil"
    (List contents) ->
      T.concat
        [ "(",
          T.unwords $ showVal <$> contents,
          ")"
        ]
    (Fun _) -> "(internal function)"
    (Lambda _ _) -> "(lambda function)"
