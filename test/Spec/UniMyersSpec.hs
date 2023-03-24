
module Spec.UniMyersSpec (spec) where

import Data.Diff.UniMyersShim
import Language.LSP.Types
import Test.Sandwich


spec :: TopSpec
spec = do
  describe "Single-line cases" $ do
    it "simple insertion" $ do
      utilDiff "ab" "abc" `shouldBe` ([TextDocumentContentChangeEvent (Just (Range (Position 0 2) (Position 0 2))) Nothing "c"])

    it "simple deletion" $ do
      utilDiff "abc" "ab" `shouldBe` ([TextDocumentContentChangeEvent (Just (Range (Position 0 2) (Position 0 3))) Nothing ""])
