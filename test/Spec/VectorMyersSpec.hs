{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Spec.VectorMyersSpec (spec) where

import Control.Monad.Catch (MonadThrow)
import Data.Diff.Types
import Data.Diff.VectorMyers
import Data.Text as T
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Apply
import TestLib.Generators
import TestLib.Util


spec :: TopSpec
spec = describe "VectorMyers" $ do
  describe "Single deletes" $ do
    checkDiff "a" "" [mkDelete (0, 0) (0, 1)]
    checkDiff "ab" "a" [mkDelete (0, 1) (0, 2)]
    checkDiff "ab" "b" [mkDelete (0, 0) (0, 1)]
    checkDiff "\na" "\n" [mkDelete (1, 0) (1, 1)]
    checkDiff "\n\na" "\n\n" [mkDelete (2, 0) (2, 1)]

  describe "Single inserts" $ do
    checkDiff "" "a" [mkInsert (0, 0) (0, 0) "a"]
    checkDiff "\n" "a\n" [mkInsert (0, 0) (0, 0) "a"]
    checkDiff "\n" "\na" [mkInsert (1, 0) (1, 0) "a"]

  describe "Double deletes" $ do
    checkDiff "ab" "" [mkDelete (0, 0) (0, 2)]
    checkDiff "xab" "x" [mkDelete (0, 1) (0, 2), mkDelete (0, 1) (0, 2)]
    checkDiff "abc" "b" [mkDelete (0, 0) (0, 1), mkDelete (0, 1) (0, 2)]

  describe "Double inserts" $ do
    checkDiff "" "ab" [mkInsert (0, 0) (0, 0) "ab"]
    checkDiff "x" "xab" [mkInsert (0, 1) (0, 1) "a", mkInsert (0, 2) (0, 2) "b"]

  describe "QuickCheck" $ introduceQuickCheck $ modifyMaxSuccess (const 100n00) $ do
    describe "Arbitrary text" $ do
      prop "Single change" $ \(InsertOrDelete (from, to)) -> verifyDiff from to
      prop "Multiple changes" $ \(MultiInsertOrDelete (from, to)) -> verifyDiff from to

    describe "Arbitrary document (series of arbitrary texts with plenty of newlines)" $ do
      prop "Single change" $ \(DocInsertOrDelete (from, to)) -> verifyDiff from to
      prop "Multiple changes" $ \(DocMultiInsertOrDelete (from, to)) -> verifyDiff from to


checkDiff :: MonadThrow m => Text -> Text -> [ChangeEvent] -> SpecFree context m ()
checkDiff from to changes = it (show from <> " -> " <> show to) $ do
  -- Check that the given changes actually work
  applyChangesText changes from `shouldBe` to

  -- Diff produces the desired changse
  diffTextsToChangeEvents from to `shouldBe` changes

verifyDiff :: Text -> Text -> Bool
verifyDiff from to = applyChangesText change from == to
  where change = diffTextsToChangeEvents from to

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
