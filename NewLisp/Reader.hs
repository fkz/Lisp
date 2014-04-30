{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module NewLisp.Reader where

import TypedErrorHandler
import NewLisp.Ast
import NewLisp.Environment
import Data.Typeable
import Data.Functor
import NewLisp.Special
import Data.Char

whitespace :: Char -> Bool
whitespace c = case c of
                 ' ' -> True
                 '\t' -> True
                 otherwise -> False


str :: Monad m => String -> LispT r m [Lisp]
str "" = return []
str a = getSymbol (reverse a) >>= \g -> return [Sym g]

data NotFinnishedReading = NotFinnishedReading deriving Typeable
instance IsSignal NotFinnishedReading String

read_ebene :: Monad m => String -> String -> Bool -> LispT r m ([Lisp], String)
read_ebene a (')':r) _ = str a >>= \x -> return (x, r)
read_ebene a (white:r) b | whitespace white = do
  (l,s) <- read_ebene "" r b
  q <- str a
  return (q ++ l, s)
read_ebene a ('(':r) b = do
  v1 <- str a
  (arr,n) <- read_ebene "" r True
  (qr,f) <- read_ebene "" n b
  return (v1 ++ foldr Cdr Empty arr : qr, f)
read_ebene a ('\'':r) b = do
  (l1:rest,s) <- read_ebene "" r b
  q <- str a
  return (q ++ [Quote l1] ++ rest, s)
read_ebene a ('`':r) b = do
  (l1:rest,s) <- read_ebene "" r b
  q <- str a
  return (q ++ [halfQuote l1] ++ rest, s)
read_ebene a (',':r) b = do
  (l1:rest,s) <- read_ebene "" r b
  q <- str a
  return (q ++ [unQuote l1] ++ rest, s)
read_ebene a ('@':r) b = do
  (l1:rest,s) <- read_ebene "" r b
  q <- str a
  return (q ++ [unQuoteList l1] ++ rest, s)
read_ebene a ('#':r) b = read_ebene a [] b
read_ebene "" q@(z:r) b | isDigit z = let (number, rest) = readInt 0 q in do
                                      (a, b) <- read_ebene "" rest b
                                      return (Lit (Int number) : a, b)
     where readInt a (z:q) | isDigit z = readInt (10 * a + toInteger (digitToInt z)) q  
           readInt a b = (a, b) 
read_ebene a (k:r) b = read_ebene (k:a) r b
read_ebene a [] True = signal NotFinnishedReading >>= flip (read_ebene a) True . (' ' :)
read_ebene a [] False = str a >>= \x -> return (x, [])

readM :: Monad m => String -> LispT r m Lisp
-- readM (')':_) = return Empty
readM str = do
  (a,rest) <- read_ebene "" str True
  return $ foldr Cdr Empty a

readR :: Monad m => String -> LispT r m Lisp
readR s = readM (s ++ ")")

readS :: Monad m => String -> LispT r m (Maybe Lisp)
readS = (headMaybe . fst <$>) . flip (read_ebene "") False
  where
    headMaybe [] = Nothing
    headMaybe (a:_) = Just a

printLisp :: Monad m => Lisp -> LispT r m String
printLisp Empty = return ""
printLisp (Sym s) = getReadableUniqueName s
printLisp (Lit l) = return $ show l
printLisp (Cdr a b) = do
  aa <- printLisp a
  bb <- printLispListe b
  return $ "(" ++ aa ++ bb
printLisp (Quote a) = do
  aa <- printLisp a
  return $ '\'' : aa

printLispListe :: Monad m => Lisp -> LispT r m String
printLispListe Empty = return ")"
printLispListe (Sym s) = (\t -> return (" : " ++ t ++ ")")) =<< printLisp (Sym s)
printLispListe (Lit l) = return $ " : " ++  show l ++ ")"
printLispListe (Cdr a b) = do
  aa <- printLisp a
  bb <- printLispListe b
  return $ " " ++ aa ++ bb
printLispListe (Quote a) = printLisp a >>= \l -> (return $ " : '" ++ l ++ ")")

