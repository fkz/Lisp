{-# LANGUAGE DeriveFunctor #-}

module Lisp where

import Prelude hiding (foldr)
import Control.Comonad
import Control.Monad
import Data.Foldable

import Data.Maybe

newtype Symbol = Symbol Integer
    deriving (Eq, Ord, Show)

data AnnotatedLisp_ a = Literal Literal | Sym Symbol | Quote (AnnotatedLisp a) | 
                     Cdr (AnnotatedLisp a) (AnnotatedLisp a) | Empty
   deriving Functor

isCdr :: AnnotatedLisp a -> Bool
isCdr (AnnotatedLisp _ (Cdr _ _)) = True
isCdr _ = False

isQuote :: AnnotatedLisp a -> Bool
isQuote (AnnotatedLisp _ (Quote _)) = True
isQuote _ = False


data AnnotatedLisp a = AnnotatedLisp a (AnnotatedLisp_ a)
  deriving Functor

data Literal = Int Integer
  deriving Show

type Lisp = AnnotatedLisp ()


data Above a = AboveQuote a
              |AboveLeftCdr a (AnnotatedLisp a)
              |AboveRightCdr a (AnnotatedLisp a)
 deriving Functor


data LispZipper a = LispZipper [Above a] (AnnotatedLisp a)
  deriving Functor

actual :: LispZipper a -> AnnotatedLisp a
actual (LispZipper _ q) = q

createLispZipper :: AnnotatedLisp a -> LispZipper a
createLispZipper al = LispZipper [] al

moveUp :: LispZipper a -> Maybe (LispZipper a)
moveUp (LispZipper [] _) = Nothing
moveUp (LispZipper (AboveQuote a:rest) l) =
    Just $ LispZipper rest (AnnotatedLisp a (Quote l))
moveUp (LispZipper (AboveLeftCdr a right : rest) left) =
    Just $ LispZipper rest $ AnnotatedLisp a (Cdr left right)
moveUp (LispZipper (AboveRightCdr a left : rest) right) =
    Just $ LispZipper rest $ AnnotatedLisp a (Cdr left right)

moveFront :: LispZipper a -> AnnotatedLisp a
moveFront lz = case moveUp lz of
                 Nothing -> actual lz
                 Just q  -> moveFront q

moveLeftDown :: LispZipper a -> LispZipper a
moveLeftDown (LispZipper above (AnnotatedLisp a (Cdr b c))) =
    LispZipper (AboveLeftCdr a c : above) b

moveRightDown :: LispZipper a -> LispZipper a
moveRightDown (LispZipper above (AnnotatedLisp a (Cdr b c))) =
    LispZipper (AboveRightCdr a b : above) c


moveQuoteDown :: LispZipper a -> LispZipper a
moveQuoteDown (LispZipper above (AnnotatedLisp a (Quote q))) =
    LispZipper (AboveQuote a : above) q

moveNext :: LispZipper a -> Maybe (LispZipper a)
moveNext q | isCdr (actual q) = Just $ moveLeftDown q
moveNext q | isQuote (actual q) = Just $ moveQuoteDown q
moveNext q = moveNextUp q

moveNextUp :: LispZipper a -> Maybe (LispZipper a)
moveNextUp q@(LispZipper (AboveQuote _ : _) _) = moveNextUp $ fromJust $ moveUp q
moveNextUp q@(LispZipper (AboveLeftCdr _ _ : _) _) = Just $ moveRightDown $ fromJust $ moveUp q
moveNextUp q@(LispZipper (AboveRightCdr _ _ : _) _) = moveNextUp $ fromJust $ moveUp q   
moveNextUp q@(LispZipper [] _) = Nothing


repeatUntil :: a -> (a -> Maybe a) -> [a]
repeatUntil start fun = start : case fun start of
                                  Just q -> repeatUntil q fun
                                  Nothing -> []


instance Foldable AnnotatedLisp where
    foldMap f lisp = foldMap f $ map extract $ repeatUntil (createLispZipper lisp) moveNext



instance Comonad LispZipper where
    extract (LispZipper _ (AnnotatedLisp a _)) = a


-- zipper
class Zipper t where
    modifyValue :: (a -> a) -> t a -> t a
    modifyValue g t = flip setValue t $ g $ getValue t

    setValue :: a -> t a -> t a
    setValue = modifyValue . const

    getValue :: t a -> a



instance Zipper LispZipper where
    modifyValue g (LispZipper above (AnnotatedLisp a r)) = LispZipper above (AnnotatedLisp (g a) r)
    getValue = extract


--moveNext q@(LispZipper ((AboveQuote _) : _) _) = moveNext (fromJust (moveUp q))
--moveNext q@(LispZipper ((


--moveUp (LispZipper AboveAndNext TopLeft above a right) left) = 
--                        Just $ LispZipper above (AnnotatedLisp a (Cdr left right))
--moveUp (LispZipper (AboveAndNext TopRight above a left) right) = 
--                        Just $ LispZipper above (AnnotatedLisp a (Cdr left right))






--instance Comonad LispZipper where
--    extract (LispZipper _ (AnnotatedLisp a _)) = a
--    duplicate (LispZipper ab al) = LispZipper 
