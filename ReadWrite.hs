{-# LANGUAGE NoMonomorphismRestriction #-}

module ReadWrite where

import Reader
import Printer
import Symbol
import Execute
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.Either

readwrite :: String -> String
readwrite s = runProgramId $ printLisp =<< readR s


initialize = basicProgram

eval :: Monad m => String -> m String
eval s = return . either show id =<< (runProgram $ runEitherT $ do
  initialize
  lisp <- lift $ readR s
  result <- execute lisp
  lift $ printLisp result)

evalId :: String -> String
evalId = runIdentity . eval

eval' :: String -> String
eval' = evalId . ("multiple " ++)
