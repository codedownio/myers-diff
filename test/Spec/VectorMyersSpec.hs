{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Spec.VectorMyersSpec (spec) where

import Control.Monad.IO.Class
import Data.Diff.Types
import Data.Diff.VectorMyers
import Data.String.Interpolate
import Data.Text as T
import Test.QuickCheck as Q
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Apply
import TestLib.Generators


spec :: TopSpec
spec = describe "VectorMyers" $ do
  it "a -> empty" $ do
    checkDiff "a" "" [mkDelete (0, 0) (0, 1)]

  it "ab -> empty" $ do
    checkDiff "ab" "" [mkDelete (0, 0) (0, 2)]

  it "ab -> a" $ do
    checkDiff "ab" "a" [mkDelete (0, 1) (0, 2)]

  it "ab -> b" $ do
    checkDiff "ab" "b" [mkDelete (0, 0) (0, 1)]

  -- it "abc -> b" $ do
  --   checkDiff "abc" "b" [mkDelete (0, 0) (0, 1), mkDelete (0, 1) (0, 2)]

  -- it "\\na -> \\n" $ do
  --   checkDiff "\na" "\n" [mkDelete (1, 0) (1, 1)]

  it "\\n\\na -> \\n\\n" $ do
    checkDiff "\n\na" "\n\n" [mkDelete (2, 0) (2, 1)]

  -- describe "Single-line cases" $ do
  --   it "simple insertion" $ do
  --     liftIO (diffTextsToChangeEvents "ab" "abc") >>= (`shouldBe` ([ChangeEvent (Range (Position 0 2) (Position 0 2)) "c"]))

  --   it "simple deletion" $ do
  --     liftIO (diffTextsToChangeEvents "abc" "ab") >>= (`shouldBe` ([ChangeEvent (Range (Position 0 2) (Position 0 3)) ""]))

  -- describe "QuickCheck" $ introduceQuickCheck $ modifyMaxSuccess (const 10000) $ do
  --   prop "Single change" $ \(InsertOrDelete (from, to)) -> verifyDiff from to
  --   prop "Multiple changes" $ \(MultiInsertOrDelete (from, to)) -> verifyDiff from to

checkDiff from to changes = do
  -- Check that the given changes actually work
  applyChangesText changes from `shouldBe` to

  -- Diff produces the desired changse
  diffTextsToChangeEvents from to `shouldBe` changes


mkDelete (l1, c1) (l2, c2) = ChangeEvent (Range (Position l1 c1) (Position l2 c2)) ""

verifyDiff from to = applyChangesText change from == to
  where change = diffTextsToChangeEvents from to

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
