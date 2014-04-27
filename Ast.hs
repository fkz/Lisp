{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ast where

import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Map.Strict
import Symbol



data Program = Program Namespace Symboltable


type GlobT c m = StateT Program (StateT [Symbol] c) m
