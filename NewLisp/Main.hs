module NewLisp.Main where

import System.Environment
import NewLisp.Repl
import System.Console.GetOpt
import Data.Maybe
import Data.Functor
import System.IO
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import TypedErrorHandler

data Flag =
  Repl | Verbose | Version | Preprocess | Load FilePath | Compile FilePath
 deriving (Show, Eq)

options :: [OptDescr Flag]
options =
    [ Option ['v']     ["verbose"] (NoArg Verbose)       "chatty output on stderr"
    , Option ['V','?'] ["version"] (NoArg Version)       "show version number"
    , Option ['p']     ["preprocess"] (NoArg Preprocess)   "preprocess the input"
    , Option ['c']     ["compile"] (ReqArg Compile "File") "compile a file (i.e. macro-expand)"
    , Option ['l']     ["load"]       (ReqArg Load "FILE")  "load file"
    , Option ['i']     ["interactive","repl"] (NoArg Repl) "get a repl environment"
    ]



main = do
  (opt', rest, errors) <- getOpt Permute options <$> getArgs

  let opt = if null opt' then [Repl] else opt'

  if not (null errors) then
      forM_ errors (hPutStrLn stderr)
   >> putStrLn (usageInfo "Usage: <Program name> options files" options)
  else do

    withLisp $ do
         forM_ opt loadFile
         if elem Preprocess opt then
             forM_ rest $ \ path -> do
                 handle <- liftIO $ openFile path ReadMode
                 finally (liftIO (hClose handle)) $
                   macroprocess handle path stdout
         else
             return ()
         
         if elem Repl opt then
             repl
         else
             return ()


           where
             loadFile (Load path) = do
                 handle <- liftIO $ openFile path ReadMode
                 finally (liftIO (hClose handle)) $
                   compileAndLoad path handle
             loadFile (Compile path) = do
                 handle <- liftIO $ openFile path ReadMode
                 handle2 <- liftIO $ openFile (path ++ ".cmp") WriteMode
                 finally (liftIO (hClose handle >> hClose handle2)) $
                   macroprocess handle path handle2
             loadFile _ = return ()
