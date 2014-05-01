{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable #-}

module NewLisp.LispError where

import NewLisp.Ast
import TypedErrorHandler
import Data.Typeable
import {-# SOURCE #-} NewLisp.Environment

data LispError =
  SymbolNotFound Symbol
 |TypeMismatch LispValue String
 |CreateNewSymbol String Symbol
 |NoExecute Symbol LispValue Lisp
 |ListNotStartWithSymbol Lisp
 |NoRealList Lisp
 |NoMacroExpansion LispExecute Lisp
 |LetParseError Lisp
 |Lisp_ArgumentListEmpty
 |SymbolNotValid Symbol
 |CantAppendToNotRealList Lisp Lisp
 |TooFewVariablesInFunctionApplication [Symbol] 
 |TooMuchVariablesInFunctionApplication [LispValue]
 |TooFewVariablesInFunctionApplicationSpecial [LispValue] (Maybe Int) (Maybe Int)
 |TooMuchVariablesInFunctionApplicationSpecial [LispValue] (Maybe Int) (Maybe Int)
 |MutableUnavailable Ptr
  deriving (Typeable, Show)


instance IsSignal LispError ()
