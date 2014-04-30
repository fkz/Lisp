{-# LANGUAGE RankNTypes, MultiParamTypeClasses, DeriveDataTypeable #-}

module NewLisp.Special where

import NewLisp.Ast
import NewLisp.Environment
import NewLisp.Execute
import NewLisp.LispError
import NewLisp.Predefined

import TypedErrorHandler

import qualified Data.Map.Strict as M
import Data.Functor
import Data.Typeable
import Control.Monad



makeSpecial :: (forall r m. Monad m => [Lisp] -> LispT r m LispValue) -> Maybe Int -> Maybe Int -> LispExecute
makeSpecial fun minParam maxParam = Special $ \ lisp -> do
                                      l <- executeList lisp
                                      ll <- assertBound l minParam maxParam
                                      fun =<< mapM (convertFromLisp "parameters ... ") ll
      where 
        assertBound :: Monad m => [LispValue] -> Maybe Int -> Maybe Int -> LispT r m [LispValue]
        assertBound l a b = (case a of
                              Just i -> if length l < i then
                                            signal $ TooFewVariablesInFunctionApplicationSpecial l a b
                                        else
                                            return () 
                              Nothing -> return ()) >>
                            (case b of
                              Just i -> if length l < i then
                                            signal $ TooMuchVariablesInFunctionApplicationSpecial l a b
                                        else
                                            return ()
                              Nothing -> return ()) >>
                            return l


  






letExpr :: Monad m => [(Symbol,LispValue)] -> LispT r m a -> LispT r m a
letExpr letVar run = do
  scope <- getScope
  let newScope = foldr (\(symbol, value) -> LexicalScope . M.insert symbol value . variables) 
                 scope letVar
  withLexicalScope newScope run

letSpecial :: LispExecute
letSpecial = Special $ \ lisp -> do
               Cdr var rest <- getCdr lisp
               vars <- lispToList var
               par <- mapM eval vars
               letExpr par (executeMany rest)
      where
        eval :: Monad m => Lisp -> LispT r m (Symbol, LispValue)
        eval l = do
          [Sym a, l] <- lispToList l -- TODO: signal if error
          (,) a <$> execute l

setSpecial :: LispExecute
setSpecial = Special $ \ lisp -> do
               [Sym var, l] <- lispToList lisp
               result <- execute l
               setVar var result
               return result

getCdr q@(Cdr a b) = return q
getCdr q = signal (LetParseError q) $> Empty
        

lambdaSpecial :: LispExecute
lambdaSpecial = Special $ \ lisp -> do
                  scope <- getScope
                  (Cdr par body) <- getCdr lisp
                  pList <- lispToList par
                  params <- mapM (convertFromLisp "lambda arg list") (map Variable pList)
                  return $ Execute $ InterpretedFunction scope params body

macroLambdaSpecial :: LispExecute
macroLambdaSpecial = Special $ \ lisp -> do
                  scope <- getScope
                  (Cdr par body) <- getCdr lisp
                  pList <- lispToList par
                  params <- mapM (convertFromLisp "macro lambda arg list") (map Variable pList)
                  return $ Macro $ InterpretedMacroFunction scope params body


quote = makeSpecial (\[val] -> return $ Variable (Quote val)) (Just 1) (Just 1)

list = makeSpecial (return . Variable . listToLisp) Nothing Nothing

list_ = makeSpecial (((<$>) Variable .) list_toLisp) (Just 1) Nothing
  where
    list_toLisp [] = signal Lisp_ArgumentListEmpty $> Empty
    list_toLisp (a : b : r) = Cdr a <$> list_toLisp (b : r)
    list_toLisp [a] = return a

concatSpecial = makeSpecial (((Variable <$>) .) concatenateLisp) Nothing Nothing

concatenateLisp :: Monad m => [Lisp] -> LispT r m Lisp
concatenateLisp [] = return Empty
concatenateLisp [a] = return a
concatenateLisp (a : r) = appendBack a =<< concatenateLisp r
  where
    appendBack Empty l = return l
    appendBack (Cdr a b) l = Cdr a <$> appendBack b l
    appendBack q l = signal (CantAppendToNotRealList q l) $> Empty

data Print = Print String
  deriving Typeable

instance IsSignal Print ()

printSpecial :: LispExecute
printSpecial = makeSpecial (\[] -> signal (Print "Hallo") $> NoValue) (Just 0) (Just 0)

halfQuoteSpecial :: LispExecute
halfQuoteSpecial = Special (return . Variable <=< qq)
    where
      qq lisp = do
        arr <- registerHandler 
                (\ s -> case s of
                          NoRealList _ -> return (Abort Nothing)
                          _ -> return NotHandled) 
               $ Just <$> lispToList lisp
        case arr of
          Nothing -> return . Quote $ lisp
          Just arri -> do
                   syms <- mapM quote2 arri
                   let sA = combineSymbols syms
                   case sA of
                     [a] -> return $ codeFor a
                     l   -> return $ listToLisp . (Sym concatSymbol :) $ map codeFor l

      quote2 (Cdr (Sym (Symbol 4)) q) = return (Left q)
      quote2 (Cdr (Sym (Symbol 8)) q) = return (Right q)
      quote2 q = Left <$> qq q

      combineSymbols :: [Either Lisp Lisp] -> [Either [Lisp] Lisp]
      combineSymbols = foldr f [Left []]
      f (Left a) (Left b : r) = Left (a : b) : r
      f (Left a) (Right b : r) = Left [a] : Right b : r
      f (Right a) (Left [] : r) = Right a : r
      f (Right a) (b : r) = Right a : b : r

      codeFor (Right a) = a
      codeFor (Left a) = listToLisp . (Sym listSymbol :) $ a
                     

macroexpandSpecial :: LispExecute
macroexpandSpecial = Special $ (Variable <$>) . compile True


catSpecial :: LispExecute
catSpecial = makeSpecial (\[Cdr a b] -> return (Variable a)) (Just 1) (Just 1)

cdrSpecial :: LispExecute
cdrSpecial = makeSpecial (\[Cdr a b] -> return (Variable b)) (Just 1) (Just 1)

listpSpecial :: LispExecute
listpSpecial = makeSpecial (\[a] -> return $ Variable $ case a of Cdr _ _ -> Lit (B True) ; _ -> Lit (B False)) (Just 1) (Just 1)


halfQuote a = Cdr (Sym halfQuoteSymbol) a
unQuote   a = Cdr (Sym unQuoteSymbol) a
unQuoteList a = Cdr (Sym unQuoteListSymbol) a
