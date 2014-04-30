{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module ParsingTree where

import qualified ArrayBuilder as B 

-- a is a list of constructors
-- b is the annotated value
data ParseTree a b = ParseTree a [ParseTree a b] b

data Zipper a b = Zipper [Above a b] (ParseTree a b)
data Above a b = Above b a [ParseTree a b] [ParseTree a b]
data Down a b = Down a Int
data Index a b = Index [Down a b]


actualIndex' :: [Above a b] -> B.Builder (Down a b)
actualIndex' (Above _ constr l1 l2 : rest) = B.concat (actualIndex' rest) (B.single (Down constr (length l1)))

actualIndex = B.finnish . actualIndex'


moveUp :: Zipper a b -> Maybe ((Zipper a b), Down a b)
moveUp (Zipper [] _) = Nothing
moveUp (Zipper (Above value constr left right : rest) actual) = 
    Just (Zipper rest (ParseTree constr (left ++ actual : right) value),
          Down constr (length left))

moveDown :: Eq a => Zipper a b -> Down a b -> Maybe (Zipper a b)
moveDown (Zipper above (ParseTree constr children value)) (Down constr2 index)
         | constr == constr2 = Just $ Zipper (Above value constr (take index children) (drop (index+1) children) : above) (children !! index)
moveDown _ _ = Nothing


from :: z -> (z -> (a, [z], l)) -> ParseTree a l
from z fun = let (a, list, l) = fun z in
             ParseTree a (map (flip from fun) list) l 

to :: ParseTree a l -> (a -> [z] -> l -> z) -> z
to (ParseTree a l b) con = con a (map (flip to con) l) b

