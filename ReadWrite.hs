module ReadWrite where

import Reader
import Printer
import Symbol

readwrite :: String -> String
readwrite s = runProgram $ printLisp =<< readR s
