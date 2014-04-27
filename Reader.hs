module Reader where

import Symbol

whitespace :: Char -> Bool
whitespace c = case c of
                 ' ' -> True
                 '\t' -> True
                 otherwise -> False


str :: Monad m => String -> Program m [Lisp]
str "" = return []
str a = symbol a >>= \g -> return [Sym g]

read_ebene :: Monad m => String -> String -> Program m ([Lisp], String)
read_ebene a (')':r) = str a >>= \x -> return (x, r)
read_ebene a (white:r) | whitespace white = do
  (l,s) <- read_ebene "" r
  q <- str a
  return (q ++ l, s)
read_ebene a ('(':r) = do
  v1 <- str a
  (arr,n) <- read_ebene "" r
  (qr,f) <- read_ebene "" n
  return (v1 ++ foldr Cdr Null arr : qr, f)
read_ebene a (k:r) = read_ebene (k:a) r

readM :: Monad m => String -> Program m Lisp
-- readM (')':_) = return Null
readM str = do
  (a,rest) <- read_ebene "" str
  return $ foldr Cdr Null a

readR :: Monad m => String -> Program m Lisp
readR s = readM (s ++ ")")


