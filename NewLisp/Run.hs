{-# LANGUAGE RankNTypes, MultiParamTypeClasses, DeriveDataTypeable #-}
module NewLisp.Run where


import NewLisp.Execute
import NewLisp.Environment
import NewLisp.Special
import NewLisp.LispError
import NewLisp.Predefined
import NewLisp.Ast
import NewLisp.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans.Maybe

import TypedErrorHandler

import qualified Data.Map.Strict as M
import Data.Functor
import Data.Typeable
import System.IO
import System.IO.Error
import Control.Exception


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

