{-# LANGUAGE FlexibleContexts #-}
module Symbol where

import Prelude hiding (lookup)
import Control.Monad.State.Strict
import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Map.Strict
import Control.Monad.Identity
import {-# SOURCE #-} Execute


newtype Symbol = Symbol Integer
    deriving (Eq, Ord, Show)
data SymbolData = SymbolData { ident :: Symbol, name :: String, var :: Lisp, exec :: Maybe Exec }

allSymbols = [Symbol i | i <- [0..]]

newtype Namespace = Namespace {namespace :: Map String Symbol}
newtype Symboltable a = Symboltable {symboltable :: Map Symbol SymbolData}

data Lisp = Literal Literal | Cdr Lisp Lisp | Sym Symbol | Null | Empty
          deriving Show


data Literal = Int Integer
               deriving Show

type Program m = StateT (Symboltable ()) (StateT Namespace (StateT [Symbol] m))



newVar :: MonadState [Symbol] m => m Symbol
newVar = state (\(a:r) -> (a,r))


newUnintSymbol :: Monad m => String -> Program m Symbol
newUnintSymbol name = do
  var <- lift $ lift newVar
  let Symbol nr = var
  let nameA = name ++ "#" ++ show nr
  modify (Symboltable . insert var (SymbolData var nameA Empty Nothing) . symboltable)
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
          put $ Symboltable $ insert v (SymbolData v name Empty Nothing) st
          return v

read :: Monad m => Symbol -> Program m (Maybe SymbolData)
read s = get >>= return . lookup s . symboltable 


runProgram :: Program Identity a -> a
runProgram v = let (((q, _), _), _) = 
                       runIdentity $ runStateT (runStateT (runStateT v (Symboltable empty)) (Namespace empty)) allSymbols in
               q
