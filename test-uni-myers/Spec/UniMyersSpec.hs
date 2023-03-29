
module Spec.UniMyersSpec (spec) where

import Control.Applicative
import Data.Diff.UniMyersShim
import Data.Text as T
import Debug.Trace
import Test.QuickCheck as Q
import Test.QuickCheck.Instances.Text
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Apply
import TestLib.Generators


spec :: TopSpec
spec = describe "uni_myers" $ do
  describe "Single-line cases" $ do
    it "simple insertion" $ do
      utilDiff "ab" "abc" `shouldBe` ([ChangeEvent (Just (Range (Position 0 2) (Position 0 2))) Nothing "c"])

    it "simple deletion" $ do
      utilDiff "abc" "ab" `shouldBe` ([ChangeEvent (Just (Range (Position 0 2) (Position 0 3))) Nothing ""])

  introduceQuickCheck $ modifyMaxSuccess (const 1000) $ do
    prop "Single change" $ \(InsertOrDelete (from, to)) -> verifyDiff from to
    prop "Multiple changes" $ \(MultiInsertOrDelete (from, to)) -> verifyDiff from to


verifyDiff :: Text -> Text -> Bool
verifyDiff from to = applyChangesText change from == to
  where
    change = utilDiff (T.unpack from) (T.unpack to)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
