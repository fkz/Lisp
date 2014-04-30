module NewLisp.Predefined where

import NewLisp.Ast

symbolList :: Symbol
symbolList = Symbol 0

concatSymbol = Symbol 9

standardErrorSymbol :: Symbol
standardErrorSymbol = Symbol 1

symbolCount :: Symbol
symbolCount = Symbol 2

paramVariable :: Symbol
paramVariable = Symbol 3

halfQuoteSymbol :: Symbol
halfQuoteSymbol = Symbol 3

unQuoteSymbol :: Symbol
unQuoteSymbol = Symbol 4

unQuoteListSymbol :: Symbol
unQuoteListSymbol = Symbol 8

listSymbol = Symbol 7

restSymbol = Symbol 5
