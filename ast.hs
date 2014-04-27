{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ast where

import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Map.Strict

data Lisp = Empty | Cdr Lisp Lisp | Str Symbol


data Program = Program Namespace Symboltable


type GlobT c m = StateT Program (StateT [Symbol] c) m

