{-# LANGUAGE  RankNTypes #-}
module NewLisp.Ast where


import qualified Data.Map.Strict
import Data.Map.Strict (Map)
import Control.Monad.Trans.State.Strict
import TypedErrorHandler
import Data.Functor
import {-# SOURCE #-} NewLisp.Environment


-- a symbol is just an integer
data Symbol = Symbol Int
  deriving (Ord, Eq, Show)
newtype Ptr = Ptr Int
 deriving (Ord, Eq, Show)
data SymbolData = SymbolData { name :: String }


data Lisp = Empty
          | Cdr Lisp Lisp
          | Sym Symbol
          | Lit Literal
          | Quote Lisp
  deriving Show

data Literal = Str String | Int Integer | B Bool |  StrMap (Map String LispValue)
  deriving Show

($>) :: Functor m => m a -> b -> m b
($>) = flip (<$)
