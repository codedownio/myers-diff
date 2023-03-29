{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Spec.VectorIOMyersSpec (spec) where

import Control.Monad.IO.Class
import Data.Diff.Types
import Data.Diff.VectorIOMyers
import Data.Text as T
import Test.QuickCheck as Q
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Apply
import TestLib.Generators


spec :: TopSpec
spec = describe "VectorIOMyers" $ do
  it "a -> empty" $ do
    liftIO (diffTextsToChangeEvents "a" "") >>= (`shouldBe` ([ChangeEvent (Range (Position 0 0) (Position 0 1)) ""]))

  it "\\na -> \\n" $ do
    liftIO (diffTextsToChangeEvents "\na" "\n") >>= (`shouldBe` ([ChangeEvent (Range (Position 1 0) (Position 1 1)) ""]))

  it "\\n\\na -> \\n\\n" $ do
    liftIO (diffTextsToChangeEvents "\n\na" "\n\n") >>= (`shouldBe` ([ChangeEvent (Range (Position 2 0) (Position 2 1)) ""]))

  -- describe "Single-line cases" $ do
  --   it "simple insertion" $ do
  --     liftIO (diffTextsToChangeEvents "ab" "abc") >>= (`shouldBe` ([ChangeEvent (Range (Position 0 2) (Position 0 2)) "c"]))

  --   it "simple deletion" $ do
  --     liftIO (diffTextsToChangeEvents "abc" "ab") >>= (`shouldBe` ([ChangeEvent (Range (Position 0 2) (Position 0 3)) ""]))

  -- describe "QuickCheck" $ introduceQuickCheck $ modifyMaxSuccess (const 10000) $ do
  --   prop "Single change" $ \(InsertOrDelete (from, to)) -> verifyDiff from to
  --   prop "Multiple changes" $ \(MultiInsertOrDelete (from, to)) -> verifyDiff from to


verifyDiff :: Text -> Text -> Property
verifyDiff from to = idempotentIOProperty $ do
  change <- diffTextsToChangeEvents from to
  return (applyChangesText change from == to)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
