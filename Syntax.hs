module Syntax where

import Symbol


-- replace a symbol by lisp code
replace :: Lisp -> Symbol -> Lisp -> Lisp
replace (Sym a) b c | a == b = c
replace (Sym a) b _ | a /= b = Sym a
replace (Cdr a b) c d = Cdr (replace a c d) (replace b c d)
replace r _ _ = r
