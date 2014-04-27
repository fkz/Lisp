module Printer where

import Symbol


just (Just a) = a
printLisp :: Monad m => Lisp -> Program m String
printLisp Null = return ""
printLisp Empty = return ""
printLisp (Sym s) = do
  sd <- Symbol.read s
  return $ name $ just sd
printLisp (Literal l) = return $ show l
printLisp (Cdr a b) = do
  aa <- printLisp a
  bb <- printLispListe b
  return $ "(" ++ aa ++ bb

printLispListe :: Monad m => Lisp -> Program m String
printLispListe Null = return ")"
printLispListe Empty = return ")"
printLispListe (Sym s) = return . (" : " ++) =<< printLisp (Sym s)
printLispListe (Literal l) = return $ " : " ++  show l
printLispListe (Cdr a b) = do
  aa <- printLisp a
  bb <- printLispListe b
  return $ " " ++ aa ++ bb
