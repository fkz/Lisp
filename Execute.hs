{-# LANGUAGE Rank2Types, ExistentialQuantification, DeriveDataTypeable #-}

module Execute where

import Symbol
import Syntax
import Data.Maybe
import Data.Typeable
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class

data LispError =
  WrongParameterCount
  | NoValidList
  | SymbolExpected
  | StrangeSymbolError
  | SymbolNotAFunction

type ProgramE m = EitherT LispError (Program m)

data Exec = Exec {runExec :: forall m . Monad m => Lisp -> ProgramE m (Lisp)}
            deriving Typeable


toArray :: Lisp -> Maybe [Lisp]
toArray (Cdr a b) = return . (a :) =<< toArray b
toArray Empty = return []
toArray _  = Nothing


parametrized :: (forall m . Monad m => [Lisp] -> ProgramE m (Lisp)) -> Exec
parametrized f = Exec $ maybe (left NoValidList) return . toArray >=> f


genSym' :: Monad m => Symbol -> ProgramE m Symbol
genSym' name = lift . newUnintSymbol . Symbol.name . fromJust =<< lift (Symbol.read name)

genSym :: Exec
genSym = parametrized q where
    q [Sym name] = return . Sym =<< genSym' name
    q _ = left WrongParameterCount

function :: Monad m => Lisp -> ProgramE m Exec
function (Cdr p code) = do
  parameter <- maybe (left NoValidList) return $ toArray p >>= sequence . map toSym
  parameter2 <- mapM genSym' parameter
  let newCode = foldl f code $ zip parameter (map Sym parameter2)

  return $ parametrized $ return . foldl f newCode . zip parameter2
                     
    where
      f code' (from, to) = replace code' from to
      toSym (Sym s) = Just s
      toSym _       = Nothing


execute :: Monad m => Lisp -> ProgramE m Lisp
execute (Cdr (Sym fun) rest) = lift (Symbol.read fun) >>=
                               maybe (left StrangeSymbolError) return >>=
                               maybe (left SymbolNotAFunction) return . exec >>=
                               flip runExec rest 
