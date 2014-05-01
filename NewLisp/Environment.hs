{-# LANGUAGE RankNTypes, FlexibleInstances, TupleSections #-}

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
                 Mutable Ptr |
                 Execute LispExecute |
                 Macro LispExecute |
                 SymbolMacro
  deriving Show



data LispExecute = Special (forall r m . Monad m => Lisp -> LispT r m LispValue)
                 | InterpretedFunction LexicalScope [Symbol] Lisp
                 | InterpretedMacroFunction LexicalScope [Symbol] Lisp 


data LexicalScope = LexicalScope { variables :: Map Symbol LispValue }

data LispState = LispState { lexicalScope :: LexicalScope,
                             symbolData :: Map Symbol SymbolData,
                             mutableVar :: Map Ptr LispValue,
                             dynamicVar :: Map Symbol LispValue}

type LispT r m = SigT r (StateT LispState m)

runLispT :: Monad m => LispT r m r -> m r
runLispT l = return . fst =<< runStateT (runCodeT l) 
             (LispState (LexicalScope M.empty) M.empty M.empty M.empty)



getLexicalVar :: Monad m => Symbol -> LispT r m LispValue
getLexicalVar s = do
  ls <- gets (variables . lexicalScope)
  case M.lookup s ls of
    Nothing -> signal (SymbolNotFound s) $> NoValue
    Just x  -> return x

getDynamicVar :: Monad m => Symbol -> LispT r m LispValue
getDynamicVar s = maybe (signal (SymbolNotFound s) $> NoValue) return 
                =<< gets (M.lookup s . dynamicVar)

getVar :: Monad m => Symbol -> LispT r m LispValue
getVar s = handleIf (return . isSymbolNotFound) 
                  (const (Abort <$> getDynamicVar s)) 
              (getLexicalVar s)
  where
    isSymbolNotFound (SymbolNotFound _) = Just ()
    isSymbolNotFound _ = Nothing


setVar :: Monad m => Symbol -> LispValue -> LispT r m ()
setVar k v = state $ \ s -> ((), 
               s{lexicalScope = LexicalScope $ M.insert k v (variables (lexicalScope s))})

setDynamicVar :: Monad m => Symbol -> LispValue -> LispT r m ()
setDynamicVar k v = state $ \ s -> ((),)
                    s{dynamicVar = M.insert k v (dynamicVar s)}


newSymbol :: Monad m => String -> LispT r m Symbol
newSymbol s = do
  currentMax <- getVar symbolCount >>= convertFromLisp "expected integer in SymbolCount (2)"
  setVar symbolCount (convertToLisp (currentMax + 1))
  let newSymbol = Symbol currentMax
  setSymbolData (SymbolData s) newSymbol
  return newSymbol

setSymbolData :: Monad m => SymbolData -> Symbol -> LispT r m ()
setSymbolData d symbol = do
  state $ \ s -> ((), s{ symbolData = M.insert symbol d (symbolData s) })

getName :: Monad m => Symbol -> LispT r m String
getName s = do
  d <- gets symbolData
  case M.lookup s d of
    Just j -> return (name j)
    Nothing -> signal (SymbolNotValid s) $> ""

getReadableUniqueName :: Monad m => Symbol -> LispT r m String
getReadableUniqueName s@(Symbol r) = do
  name <- getName s
  s' <- getSymbol name
  if s == s' then
      return name
  else
      return $ "#" ++ show r ++ name


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
getScope = lexicalScope <$> get
  
withLexicalScope :: Monad m => LexicalScope -> LispT r m a -> LispT r m a
withLexicalScope scope run = do
    before <- get

    finally (do
              now <- get
              put now{ lexicalScope = lexicalScope before })
      $ do
        put before{ lexicalScope = scope }
        run


listToLisp :: [Lisp] -> Lisp
listToLisp = foldr Cdr Empty

lispToList :: Monad m => Lisp -> LispT r m [Lisp]
lispToList (Cdr a b) = (:) a <$> lispToList b
lispToList Empty = return []
lispToList a = signal (NoRealList a) $> []


readMutable :: Monad m => Ptr -> LispT r m LispValue
readMutable a = do
  map <- gets mutableVar
  case M.lookup a map of
    Just v -> return v
    Nothing -> signal (MutableUnavailable a) $> NoValue

writeMutable :: Monad m => Ptr -> LispValue -> LispT r m ()
writeMutable a v = 
  state $ \ s -> ((),) s{ mutableVar = M.insert a v (mutableVar s) }

newMutable :: Monad m => LispT r m Ptr
newMutable = gets (Ptr . M.size . mutableVar)


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
    convertFromLisp m (Mutable s) = convertFromLisp m =<< readMutable s
    convertFromLisp s v = signal (TypeMismatch v s) $> Empty
    convertToLisp = Variable

instance FromToLisp LispExecute where
    convertFromLisp _ (Execute s) = return s
    convertFromLisp s v = signal (TypeMismatch v s) $> undefined
    convertToLisp = Execute

instance FromToLisp String where
    convertFromLisp _ (Variable (Lit (Str s))) = return s
    convertFromLisp _ (Variable (Sym s)) = getName s
    convertFromLisp s v = signal (TypeMismatch v s) $> ""
    convertToLisp = Variable . Lit . Str
