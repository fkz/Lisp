module NewLisp.Execute where

import NewLisp.Ast
import NewLisp.Environment
import TypedErrorHandler
import NewLisp.LispError
import NewLisp.Predefined

import Data.Functor
import Control.Applicative
import Control.Monad


execute :: Monad m => Lisp -> LispT r m LispValue
execute (Sym s) = getVar s
execute lisp@(Cdr (Sym a) b) = do
  f <- getVar a
  case f of
    Execute le -> executeLisp le b
    Macro le -> signal (NoMacroExpansion le lisp) $> NoValue
    _ -> signal (NoExecute a f b) $> NoValue

execute lisp@(Cdr _ _) = signal (ListNotStartWithSymbol lisp) $> NoValue
execute Empty = return (Variable Empty)
execute (Lit l) = return (Variable (Lit l))
execute (Quote l) = return (Variable l)

executeLisp :: Monad m => LispExecute -> Lisp -> LispT r m LispValue
executeLisp (Special f) p = f p
executeLisp (InterpretedFunction scope params body) param = interpreteFunction scope body params param
executeLisp (InterpretedMacroFunction scope params body) param = interpreteFunctionMacro scope body params param

interpreteFunction :: Monad m => LexicalScope -> Lisp -> [Symbol] -> Lisp -> LispT r m LispValue
interpreteFunction scope fun param_names param = do
  params <- executeList param
  withLexicalScope scope $
   introduceParams param_names params  >> executeMany fun

interpreteFunctionMacro :: Monad m => LexicalScope -> Lisp -> [Symbol] -> Lisp -> LispT r m LispValue
interpreteFunctionMacro scope fun param_names param = do
  params <- lispToList param
  withLexicalScope scope $ 
   introduceParams param_names (Variable <$> params) >> executeMany fun

introduceParams :: Monad m => [Symbol] -> [LispValue] -> LispT r m ()
introduceParams [] [] = return ()
introduceParams (rest : s : []) r | rest == restSymbol = 
  setVar s =<< Variable . listToLisp <$> mapM (convertFromLisp "rest arguments have to be Lisp") r
introduceParams (s : r) (v : r') = setVar s v >> introduceParams r r'
introduceParams s [] = signal (TooFewVariablesInFunctionApplication s)
introduceParams [] s = signal (TooMuchVariablesInFunctionApplication s)


-- bool : is top level ?
compile :: Monad m => Bool -> Lisp -> LispT r m Lisp
compile True l@(Cdr q@(Sym a) b) = macroexpand l
compile _ (Cdr l@(Cdr q@(Sym a) b) r) = flip Cdr <$> compile False r <*> macroexpand l
compile _ (Cdr a b) = Cdr <$> compile False a <*> compile False b
compile _ q = return q                


macroexpand (Cdr q@(Sym a) b) = do
  f <- registerHandler ifExists $ getVar a >>= return . Just
  case f of
    Just (Macro le) -> compile True  =<< convertFromLisp "macro expanding" =<< executeLisp le b
    _ -> Cdr <$> compile False q <*> compile False b
      
  where
    ifExists sig = 
        case sig of
          SymbolNotFound _ -> return (Abort Nothing)
          _ -> return NotHandled


-- some helper functions

executeList :: Monad m => Lisp -> LispT r m [LispValue]
executeList = lispToList >=> mapM execute

executeMany :: Monad m => Lisp -> LispT r m LispValue
executeMany = executeList >=> return . last
