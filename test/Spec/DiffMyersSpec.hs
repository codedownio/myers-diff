
module Spec.DiffMyersSpec (spec) where

import Control.Applicative
import Data.Diff.DiffMyersShim
import Data.Text as T
import Debug.Trace
import Language.LSP.Types
import Test.QuickCheck as Q
import Test.QuickCheck.Instances.Text
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Apply
import TestLib.Generators


spec :: TopSpec
spec = describe "diff_myers" $ do
  describe "Single-line cases" $ do
    it "simple insertion" $ do
      diffDiff "ab" "abc" `shouldBe` ([TextDocumentContentChangeEvent (Just (Range (Position 0 2) (Position 0 2))) Nothing "c"])

    it "simple deletion" $ do
      diffDiff "abc" "ab" `shouldBe` ([TextDocumentContentChangeEvent (Just (Range (Position 0 2) (Position 0 3))) Nothing ""])

  describe "QuickCheck" $ introduceQuickCheck $ modifyMaxSuccess (const 10000) $ do
    prop "Single change" $ \(InsertOrDelete (from, to)) -> verifyDiff from to
    prop "Multiple changes" $ \(MultiInsertOrDelete (from, to)) -> verifyDiff from to


verifyDiff :: Text -> Text -> Bool
verifyDiff from to = applyChangesText change from == to
  where
    change = diffDiff (T.unpack from) (T.unpack to)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
