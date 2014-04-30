module Tests.TestConcat where

import ParsingTree
import Test.QuickCheck
import Test.QuickCheck.Property ((===))
import Data.Functor
import Control.Applicative


data Compose a = Compose (Compose a) (Compose a) | Leaf a
                 deriving Show

instance Arbitrary a => Arbitrary (Compose a) where
    arbitrary = do
      value <- arbitrary :: Gen Bool
      if value then
          Leaf <$> arbitrary
      else
          Compose <$> arbitrary <*> arbitrary

composeToArray :: Compose a -> [a]
composeToArray (Leaf a) = [a]
composeToArray (Compose c1 c2) = composeToArray c1 ++ composeToArray c2
composeToBuilder :: Compose a -> Builder a
composeToBuilder (Leaf a) = single a
composeToBuilder (Compose c1 c2) = composeToBuilder c1 `concatBuilder` composeToBuilder c2


testComposeValid l = (===) (composeToArray l) (finnishBuilder (composeToBuilder l))


testParseUp z = case moveUp z of
                  Nothing -> rejected
                  Just (z', v) -> case  moveDown z' v of
                                    Nothing -> failed
                                    Just z2 -> liftBool $ z == z2
