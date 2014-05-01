module NewLisp.Repl where

import NewLisp.Run

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
import Control.Exception hiding (finally)
import NewLisp.ErrorMessages
import Control.Applicative
import Data.Maybe
import Control.Monad.Writer
import Control.Monad


nextRepl :: MonadIO m => LispT r m ()
nextRepl = do
  liftIO $ putStr ">" >> hFlush stdout
  str <- liftIO $ handleJust (boolToMaybe . isEOFError) (return . const "quit") getLine
  if str == "quit" then
      signal Quit
  else if str == "abort" then
     return ()
  else 
      registerHandler
           (prettyPrintLispContext >=> (Abort () <$) . liftIO . hPutStr stderr) $
      registerHandlerT NotFinnishedReading 
           (const $ liftIO $ putStr "==>" >> hFlush stdout >> Continue <$> getLine) $
      addContext TopContext $ 
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


compileFile' :: MonadIO m => Handle -> LispT r (StateT Int m) [(Int, Lisp)]
compileFile' h = liftIO (hIsEOF h) >>= \r -> if r then return [] else do
  str <- liftIO (hGetLine h)
  lift . lift$ modify (+1)  
  current <- lift . lift $ get
  (++) <$> (addContext (LineContext current) $
            registerHandlerT NotFinnishedReading 
            (const ((lift . lift) (modify (+1)) >> Continue <$> liftIO (hGetLine h))) $ do
              a <- readS str
              case a of
                Nothing -> return []
                Just q -> (\a -> [(current, a)]) <$> compile True q ) <*> compileFile' h

compileWFile :: MonadIO m => String -> Handle -> LispT r (StateT Int m) [(Int, Lisp)]
compileWFile filename h = 
    (lift . lift) (put 0)   -- set line number to 0
 >> addContext (FileContext filename)
      (compileFile' h)

compileFile :: MonadIO m => String -> Handle -> LispT r (StateT Int m) [Lisp]
compileFile s = (map snd <$>) . compileWFile s


load :: MonadIO m => String -> [(Int, Lisp)] -> LispT r m ()
load filename code =
    addContext (FileContext filename) $
      forM_ code $ \(nr, lsp) ->
         addContext (LineContext nr) $
            execute lsp

compileAndLoad ::  MonadIO m => String -> Handle -> LispT r (StateT Int m) ()
compileAndLoad filename =
    compileWFile filename >=> load filename

repl :: MonadIO m => LispT r m ()
repl = registerHandlerT Quit (const $ return $ Abort ()) $
         forever nextRepl

withLisp :: LispT () (StateT Int IO) () -> IO ()
withLisp c = flip evalStateT 0 $ runLispT $ 
               startEnvironment >>
               registerHandler
                 (prettyPrintLispContext >=> (Abort () <$) . liftIO . hPutStr stderr)
                  c


-- macro process a file a with name b; put result into file c
macroprocess :: MonadIO m => Handle -> String -> Handle -> LispT r (StateT Int m) ()
macroprocess handle filename to = 
     compileFile filename handle
  >>= mapM_ (liftIO . hPutStrLn to <=< printLisp)            
                 
                 
