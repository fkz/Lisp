{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, FlexibleInstances, Rank2Types #-}
module LispEnvironment where

import TypedErrorHandler
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Control.Monad.State.Strict
import Control.Monad.State.Class
import Data.Typeable
import Control.Applicative
import Data.Functor
import Control.Monad.Identity
import qualified ArrayBuilder as B
import Data.Char
import System.IO
import Control.Monad.Trans.Maybe
import Control.Exception hiding (finally)
import System.IO.Error

-- a symbol is just an integer
data Symbol = Symbol Int
  deriving (Ord, Eq, Show)
data SymbolData = SymbolData { name :: String }


-- the currently available symbols
data LexicalScope = LexicalScope { variables :: Map Symbol LispValue }


data Lisp = Empty
          | Cdr Lisp Lisp
          | Sym Symbol
          | Lit Literal
          | Quote Lisp
  deriving Show

data Literal = Str String | Int Integer | B Bool |  StrMap (Map String LispValue)
  deriving Show

-- a value stored in a variable
data LispValue = NoValue | 
                 Variable Lisp |
                 Execute LispExecute |
                 Macro LispExecute |
                 SymbolMacro
  deriving Show

data LispExecute = Special (forall r m . Monad m => Lisp -> LispT r m LispValue) | --TODO: get r m more front
                   InterpretedFunction LexicalScope [Symbol] Lisp
                  |InterpretedMacroFunction LexicalScope [Symbol] Lisp 

instance Show LispExecute where
    show (Special _) = "<Special>"
    show (InterpretedFunction _ p l) = "InterpretedFunction <..> " ++ show p ++ " -> " ++  show l
    show (InterpretedMacroFunction _ p l) = "InterpretedMacroFunction <..> " ++ show p ++ "->" ++ show l

type LispT r m = SigT r (StateT (LexicalScope, Map Symbol SymbolData) m)


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
  deriving (Typeable, Show)

instance IsSignal LispError ()

($>) :: Functor f => f b -> a -> f a
($>) = flip (<$)

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

lispToList :: Monad m => Lisp -> LispT r m [Lisp]
lispToList (Cdr a b) = (:) a <$> lispToList b
lispToList Empty = return []
lispToList a = signal (NoRealList a) $> []


executeList :: Monad m => Lisp -> LispT r m [LispValue]
executeList = lispToList >=> mapM execute

executeMany :: Monad m => Lisp -> LispT r m LispValue
executeMany = executeList >=> return . last


makeSpecial :: (forall r m. Monad m => [Lisp] -> LispT r m LispValue) -> Maybe Int -> Maybe Int -> LispExecute
makeSpecial fun minParam maxParam = Special $ \ lisp -> do
                                      l <- executeList lisp
                                      ll <- assertBound l minParam maxParam
                                      fun =<< mapM (convertFromLisp "") ll
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

quote :: LispExecute
quote = Special $ \ lisp -> do
          [val] <- lispToList lisp
          Variable <$> (Quote <$> (convertFromLisp "can only quote lisp" =<< execute val)) 

list :: LispExecute
list = Special $ \ lisp -> do
         Variable . listToLisp <$> (executeList lisp >>= mapM (convertFromLisp "list paramater must evaluate to Lisp"))

list_ :: LispExecute
list_ = Special $ \ lisp -> do
          args <- executeList lisp >>= mapM (convertFromLisp "list* parameter must evaluate to Lisp")
          Variable <$> list_toLisp args
  where
    list_toLisp [] = signal Lisp_ArgumentListEmpty $> Empty
    list_toLisp (a : b : r) = Cdr a <$> list_toLisp (b : r)
    list_toLisp [a] = return a

concatSpecial :: LispExecute
concatSpecial = Special $ \ lisp -> do
                  args <- executeList lisp >>= mapM (convertFromLisp "concatenate parameter must evaluate to Lisp")
                  Variable <$> concatenateLisp args


concatenateLisp :: Monad m => [Lisp] -> LispT r m Lisp
concatenateLisp [] = return Empty
concatenateLisp [a] = return a
concatenateLisp (a : r) = appendBack a =<< concatenateLisp r
  where
    appendBack Empty l = return l
    appendBack (Cdr a b) l = Cdr a <$> appendBack b l
    appendBack q l = signal (CantAppendToNotRealList q l) $> Empty

listToLisp :: [Lisp] -> Lisp
listToLisp = foldr Cdr Empty

data Print = Print String
  deriving Typeable

instance IsSignal Print ()

printSpecial :: LispExecute
printSpecial = Special $ \ lisp -> signal (Print "Hallo") $> NoValue

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

halfQuote a = Cdr (Sym halfQuoteSymbol) a
unQuote   a = Cdr (Sym unQuoteSymbol) a
unQuoteList a = Cdr (Sym unQuoteListSymbol) a

addFunction :: String -> LispExecute -> Monad m => LispT r m ()
addFunction s l = do
  symbol <- getSymbol s
  setVar symbol (Execute l)


startEnvironment :: Monad m => LispT r m ()
startEnvironment = do
  setVar symbolList (convertToLisp (M.fromList startList))
  setVar standardErrorSymbol NoValue
  setVar symbolCount (convertToLisp (10 :: Int))
  setVar paramVariable NoValue

  mapM_ (\(n, s) -> setSymbolData (SymbolData n) s) startList'

  mapM_ (uncurry addFunction) funs 

  setVar halfQuoteSymbol (Macro halfQuoteSpecial)


    where
      startList = flip map startList' $ \(a, b) -> (a, Variable $ Sym b)
      startList' = [("SymbolList", symbolList),
                   ("SymbolCount", symbolCount),
                   ("$", paramVariable),
                   ("list", listSymbol),
                   ("`", halfQuoteSymbol),
                   ("@", unQuoteListSymbol),
                   (",", unQuoteSymbol),
                   ("concat", concatSymbol),
                   ("&rest", restSymbol)]

      funs = [("printS", printSpecial),
              ("let", letSpecial),
              ("lambda", lambdaSpecial),
              ("macrolambda", macroLambdaSpecial),
              ("set", setSpecial),
              ("quote", quote),
              ("list", list),
              ("list*", list_),
              ("concat", concatSpecial),
              ("macroexpand", macroexpandSpecial)]

runLispT :: Monad m => LispT r m r -> m r
runLispT l = return . fst =<< runStateT (runCodeT l) (LexicalScope M.empty, M.empty)


runLisp :: LispT (Either LispError r) Identity r -> Either LispError r
runLisp = runIdentity . runLispT . registerHandler handler . liftM Right
  where handler = return . Abort . Left


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

run :: String -> Either LispError LispValue
run s = runLisp $ startEnvironment >> readR s >>= compile False >>= executeMany

data Quit = Quit deriving Typeable
instance IsSignal Quit ()

boolToMaybe :: Bool -> Maybe ()
boolToMaybe True = Just ()
boolToMaybe False = Nothing

nextRepl :: LispT r IO ()
nextRepl = do
  liftIO $ putStr ">" >> hFlush stdout
  str <- liftIO $ handleJust (boolToMaybe . isEOFError) (return . const "quit") getLine
  if str == "quit" then
      signal Quit
  else if str == "abort" then
     return ()
  else 
      registerHandlerT (undefined :: LispError)
           ((Abort () <$) . liftIO . print) $
      registerHandlerT NotFinnishedReading 
           (const $ liftIO $ putStr "==>" >> hFlush stdout >> Continue <$> getLine) $
      void $ runMaybeT $
       MaybeT (readS str) >>= 
       lift . compile True >>= 
       lift . execute >>= 
       lift . lispToString >>= 
       liftIO . putStr >> 
       liftIO (putStrLn "")
          where
            lispToString (Variable w) = printLisp w
            lispToString q = return $ show q



repl :: IO ()
repl = runLispT $ do
         startEnvironment
         registerHandlerT Quit (const $ return $ Abort ()) $
           forever nextRepl

