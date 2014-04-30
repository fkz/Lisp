module ArrayBuilder
    (Builder,
     concat,
     empty,
     single,
     finnish,
     showB) where

import Prelude hiding (concat)

newtype Builder a = Builder {runBuilder :: [a] -> [a]}

concat :: Builder a -> Builder a -> Builder a
concat (Builder f) (Builder g) = Builder $ g . f

empty :: Builder a
empty = Builder id

single :: a -> Builder a
single a = Builder $ \ prep -> a : prep 

finnish :: Builder a -> [a]
finnish b = runBuilder b []

showB :: Show a => a -> Builder Char
showB = Builder . shows
