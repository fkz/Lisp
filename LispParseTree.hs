module LispParseTree where

import ParsingTree

data Symbol
data Literal

data Lisp = Sym Symbol
          | Empty
          | Cdr Lisp Lisp
          | Literal Literal


data LispConstructors = CSym | CEmpty | CLiteral | CCdr
  deriving Enum
          
