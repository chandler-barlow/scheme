{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval where

import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Text as T
import LispVal
import Parser

basicEnv :: Map.Map T.Text LispVal
basicEnv = Map.fromList $ primEnv <> [("read", Fun $ IFunc $ unop $ readFn)]

evalFile :: T.Text -> IO ()
evalFile fileExpr =
  (runASTinEnv basicEnv $ fileToEvalForm fileExpr) >>= print

fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input =
  either (throw . PError . show) evalBody $ readExprFile input

runParseTest :: T.Text -> T.Text
runParseTest input =
  either (T.pack . show) (T.pack . show) $ readExpr input

runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action = runResourceT $ runReaderT (runEval action) code
