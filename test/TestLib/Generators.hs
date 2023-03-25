
module TestLib.Generators where

import Data.Text as T
import Test.QuickCheck as Q
import Test.QuickCheck.Instances.Text ()


newtype InsertOrDelete = InsertOrDelete (Text, Text)
  deriving (Show, Eq)
instance Arbitrary InsertOrDelete where
  arbitrary = InsertOrDelete <$> oneof [arbitraryInsert, arbitraryDelete]

arbitraryInsert :: Gen (Text, Text)
arbitraryInsert = do
  initial <- arbitrary
  toInsert <- arbitrary

  pos <- chooseInt (0, T.length initial)
  let (x, y) = T.splitAt pos initial

  return (initial, x <> toInsert <> y)

arbitraryDelete :: Gen (Text, Text)
arbitraryDelete = do
  initial <- arbitrary

  pos1 <- chooseInt (0, max 0 (T.length initial - 1))
  pos2 <- chooseInt (pos1, T.length initial)

  let (x, y) = T.splitAt pos1 initial
  let (_, z) = T.splitAt (pos2 - pos1) y

  return (initial, x <> z)
