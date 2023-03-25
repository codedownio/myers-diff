
module Spec.UniMyersSpec (spec) where

import Control.Applicative
import Data.Diff.UniMyersShim
import Data.Text as T
import Language.LSP.Types
import Test.QuickCheck as Q
import Test.QuickCheck.Instances.Text
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Apply
import TestLib.Generators


spec :: TopSpec
spec = do
  describe "Single-line cases" $ do
    it "simple insertion" $ do
      utilDiff "ab" "abc" `shouldBe` ([TextDocumentContentChangeEvent (Just (Range (Position 0 2) (Position 0 2))) Nothing "c"])

    it "simple deletion" $ do
      utilDiff "abc" "ab" `shouldBe` ([TextDocumentContentChangeEvent (Just (Range (Position 0 2) (Position 0 3))) Nothing ""])

  introduceQuickCheck $ do
    prop "Arbitrary single change" $ (\(InsertOrDelete (from, to)) -> verifyDiff from to)


verifyDiff from to = applyChangesText change from == to
  where
    change = utilDiff (T.unpack from) (T.unpack to)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
