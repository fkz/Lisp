{-# LANGUAGE FlexibleContexts #-}
module Symbol where

import Prelude hiding (lookup)
import Control.Monad.State.Strict
import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Map.Strict
import Control.Monad.Identity
import Data.Maybe
import Lisp
import {-# SOURCE #-} Execute


newtype Symbol = Symbol Integer
    deriving (Eq, Ord, Show)
data SymbolData = SymbolData { ident :: Symbol, name :: String, var :: Lisp, exec :: Maybe Exec }

allSymbols = [Symbol i | i <- [0..]]

newtype Namespace = Namespace {namespace :: Map String Symbol}
newtype Symboltable a = Symboltable {symboltable :: Map Symbol SymbolData}


type Program m = StateT (Symboltable ()) (StateT Namespace (StateT [Symbol] m))



newVar :: MonadState [Symbol] m => m Symbol
newVar = state (\(a:r) -> (a,r))


newUnintSymbol :: Monad m => String -> Program m Symbol
newUnintSymbol name = do
  var <- lift $ lift newVar
  let Symbol nr = var
  let nameA = name ++ "#" ++ show nr
  modify (Symboltable . insert var (SymbolData var nameA Null Nothing) . symboltable)
  return var

symbol :: Monad m => String -> Program m Symbol
symbol name = do
  ns <- return . namespace =<< lift get
  case lookup name ns of
    Just y -> return y
    Nothing ->
        do
          v <- (lift . lift) newVar
          lift $ put $ Namespace $ insert name v ns
          st <- return . symboltable =<< get
          put $ Symboltable $ insert v (SymbolData v name Null Nothing) st
          return v

read :: Monad m => Symbol -> Program m (Maybe SymbolData)
read s = get >>= return . lookup s . symboltable 

setExec :: Monad m => Symbol -> Exec -> Program m ()
setExec s e = do
  (Just (SymbolData i1 n1 v1 e1))  <- Symbol.read s
  st <- return . symboltable =<< get
  put $ Symboltable $ insert s (SymbolData i1 n1 v1 (Just e)) st

setVar :: Monad m => Symbol -> Lisp -> Program m ()
setVar s v = do
  (Just (SymbolData i1 n1 v1 e1))  <- Symbol.read s
  st <- return . symboltable =<< get
  put $ Symboltable $ insert s (SymbolData i1 n1 v e1) st

getVar :: Monad m => Symbol -> Program m Lisp
getVar = Symbol.read >=> return . var . fromJust




runProgram :: Monad m => Program m a -> m a
runProgram v = do
  (((q, _), _), _) <- runStateT (runStateT (runStateT v (Symboltable empty)) (Namespace empty)) allSymbols
  return q

runProgramId :: Program Identity a -> a
runProgramId = runIdentity . runProgram
