{-# LANGUAGE RankNTypes, FlexibleInstances #-}

module NewLisp.Environment where

import NewLisp.Ast
import NewLisp.LispError
import NewLisp.Predefined

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Control.Monad.State
import TypedErrorHandler
import Data.Functor

-- a value stored in a variable
data LispValue = NoValue | 
                 Variable Lisp |
                 Execute LispExecute |
                 Macro LispExecute |
                 SymbolMacro
  deriving Show

data LispExecute = Special (forall r m . Monad m => Lisp -> LispT r m LispValue)
                 | InterpretedFunction LexicalScope [Symbol] Lisp
                 | InterpretedMacroFunction LexicalScope [Symbol] Lisp 


data LexicalScope = LexicalScope { variables :: Map Symbol LispValue }

type LispT r m = SigT r (StateT (LexicalScope, Map Symbol SymbolData) m)



getVar :: Monad m => Symbol -> LispT r m LispValue
getVar s = do
  ls <- gets (variables . fst)
  case M.lookup s ls of
    Nothing -> signal (SymbolNotFound s) $> NoValue
    Just x  -> return x

setVar :: Monad m => Symbol -> LispValue -> LispT r m ()
setVar k v = state $ \ (map, r) -> ((), (LexicalScope $ M.insert k v (variables map), r))


newSymbol :: Monad m => String -> LispT r m Symbol
newSymbol s = do
  currentMax <- getVar symbolCount >>= convertFromLisp "expected integer in SymbolCount (2)"
  setVar symbolCount (convertToLisp (currentMax + 1))
  let newSymbol = Symbol currentMax
  setSymbolData (SymbolData s) newSymbol
  return newSymbol

setSymbolData :: Monad m => SymbolData -> Symbol -> LispT r m ()
setSymbolData d symbol = do
  state $ \ (a, m) -> ((), (a, M.insert symbol d m))

getName :: Monad m => Symbol -> LispT r m String
getName s = do
  (_, d) <- get
  case M.lookup s d of
    Just j -> return (name j)
    Nothing -> signal (SymbolNotValid s) $> ""

getReadableUniqueName :: Monad m => Symbol -> LispT r m String
getReadableUniqueName s = do
  name <- getName s
  s' <- getSymbol name
  if s == s' then
      return name
  else
      return $ "#" ++ show s ++ name


-- the reader : is saved in the 0-th symbol
getSymbol :: Monad m => String -> LispT r m Symbol
getSymbol s = do
  map <- convertFromLisp "expected StrMap for Reader in Symbol 0" =<< getVar symbolList 
  case M.lookup s map of
    Just symbol -> convertFromLisp ("expected Symbol in StrMap at symbol " ++ s) symbol
    Nothing -> do
      symbol <- newSymbol s
      let map' = M.insert s (convertToLisp symbol) map
      setVar symbolList (Variable (Lit (StrMap map')))
      return symbol

getScope :: Monad m => LispT r m LexicalScope
getScope = fst <$> get
  
withLexicalScope :: Monad m => LexicalScope -> LispT r m a -> LispT r m a
withLexicalScope scope run = do
    (scopeBefore,r) <- get

    finally (do
              (_,r) <- get
              put (scopeBefore, r))
      $ do
        put (scope,r)
        run


listToLisp :: [Lisp] -> Lisp
listToLisp = foldr Cdr Empty

lispToList :: Monad m => Lisp -> LispT r m [Lisp]
lispToList (Cdr a b) = (:) a <$> lispToList b
lispToList Empty = return []
lispToList a = signal (NoRealList a) $> []






instance Show LispExecute where
    show (Special _) = "<Special>"
    show (InterpretedFunction _ p l) = "InterpretedFunction <..> " ++ show p ++ " -> " ++  show l
    show (InterpretedMacroFunction _ p l) = "InterpretedMacroFunction <..> " ++ show p ++ "->" ++ show l


class FromToLisp a where
    convertFromLisp :: Monad m => String -> LispValue -> LispT r m a
    convertToLisp :: a -> LispValue

instance FromToLisp Int where
    convertFromLisp _ (Variable (Lit (Int i))) = return (fromInteger i)
    convertFromLisp s v = signal (TypeMismatch v s) $> 0
    convertToLisp = Variable . Lit . Int . toInteger

instance FromToLisp (Map String LispValue) where
    convertFromLisp _ (Variable (Lit (StrMap i))) = return i
    convertFromLisp s v = signal (TypeMismatch v s) $> M.empty
    convertToLisp = Variable . Lit . StrMap

instance FromToLisp Symbol where
    convertFromLisp _ (Variable (Sym s)) = return s
    convertFromLisp s v = signal (TypeMismatch v s) $> standardErrorSymbol
    convertToLisp = Variable . Sym

instance FromToLisp Lisp where
    convertFromLisp _ (Variable s) = return s
    convertFromLisp s v = signal (TypeMismatch v s) $> Empty
    convertToLisp = Variable
