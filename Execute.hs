{-# LANGUAGE Rank2Types, ExistentialQuantification, DeriveDataTypeable, TupleSections #-}

module Execute where

import Symbol
import Syntax
import Data.Maybe
import Data.Typeable
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class

data LispError =
  WrongParameterCount
  | NoValidList
  | SymbolExpected
  | StrangeSymbolError
  | SymbolNotAFunction Symbol
  | SymbolHasNoData Symbol
  | CantExecute
    deriving Show

type ProgramE m = EitherT LispError (Program m)

data Exec = Exec {runExec :: forall m . Monad m => Lisp -> ProgramE m (Lisp)}
            deriving Typeable


toArray :: Lisp -> Maybe [Lisp]
toArray (Cdr a b) = return . (a :) =<< toArray b
toArray Empty = return []
toArray _  = Nothing


parametrized :: (forall m . Monad m => [Lisp] -> ProgramE m (Lisp)) -> Exec
parametrized f = Exec $ maybe (left NoValidList) return . toArray >=> f


genSym' :: Monad m => Symbol -> ProgramE m Symbol
genSym' name = lift . newUnintSymbol . Symbol.name . fromJust =<< lift (Symbol.read name)

genSym :: Exec
genSym = parametrized q where
    q [Sym name] = return . Sym =<< genSym' name
    q _ = left WrongParameterCount

function :: Monad m => Lisp -> ProgramE m Exec
function (Cdr p code) = do
  parameter <- maybe (left NoValidList) return $ toArray p >>= sequence . map toSym
  
  return $ parametrized $ \arguments -> do
    parameter2 <- mapM genSym' parameter
    let newCode = foldl f code $ zip parameter (map Sym parameter2)
    args <- sequence $ zipWith (\arg par -> execute arg >>= return . (, par)) arguments parameter2
    mapM_ (\(value, symbol) -> lift $ setVar symbol value) args

    executeMany newCode
                     
    where
      f code' (from, to) = replace code' from to
      toSym (Sym s) = Just s
      toSym _       = Nothing

defun :: Exec
defun = Exec q
    where
      q (Cdr (Sym name) rest) = function rest >>= lift . setExec name >> return Null
      q _ = left NoValidList


execute :: Monad m => Lisp -> ProgramE m Lisp
execute (Cdr (Sym fun) rest) = 
    do
      funSymbol <- lift (Symbol.read fun) >>= maybe (left StrangeSymbolError) return
      exec <- maybe (left (SymbolNotAFunction fun)) return $ exec funSymbol
      --rest' <- executeCdr rest
      runExec exec rest
execute (Quote a) = return a
execute Empty = return Empty
execute (Sym s) = do
                    result <- lift $ getVar s
                    case result of
                        Null ->left (SymbolHasNoData s)
                        otherwise -> return result
execute _ = left CantExecute


executeCdr :: Monad m => Lisp -> ProgramE m Lisp
executeCdr (Cdr a b) = do
  aa <- execute a
  bb <- executeCdr b
  return $ Cdr aa bb
executeCdr Empty = return Empty
executeCdr _     = left NoValidList


executeMany :: Monad m => Lisp -> ProgramE m Lisp
executeMany l = maybe (left NoValidList) return (toArray l) >>= 
                (flip foldM Null $ \ _ b -> execute b)

multiple :: Exec
multiple = Exec executeMany
-- multiple = parametrized $ flip foldM Null $ \ _ b -> execute b


defmacro :: Exec
defmacro = Exec q
    where
      q (Cdr (Sym name) rest) = do
        f <- function rest
        lift $ setExec name $ Exec $ (runExec f >=> execute) . dotfy
        return Null
      dotfy :: Lisp -> Lisp
      dotfy (Cdr a b) = Cdr (Quote a) (dotfy b)
      dotfy r = r

list :: Exec
list = parametrized $ flip foldr (return Empty) $ \a b -> liftM2 Cdr (execute a) b

registerSymbol :: Monad m => String -> Exec -> ProgramE m ()
registerSymbol name ex = lift $ symbol (reverse name) >>= flip setExec ex 

help :: Exec
help = Exec return


basicProgram :: Monad m => ProgramE m ()
basicProgram = mapM_ (uncurry registerSymbol)
               [ ("defun", defun),
                 ("genSym", genSym),
                 ("multiple", multiple),
                 ("help", help),
                 ("defmacro", defmacro),
                 ("list", list)]
