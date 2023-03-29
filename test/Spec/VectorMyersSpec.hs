{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Spec.VectorMyersSpec (spec) where

import Control.Monad.IO.Class
import Data.Diff.Types
import Data.Diff.VectorIOMyers
import Data.String.Interpolate
import Data.Text as T
import Test.QuickCheck as Q
import Test.Sandwich
import Test.Sandwich.QuickCheck
import TestLib.Apply
import TestLib.Generators


spec :: TopSpec
spec = describe "VectorIOMyers" $ do
  it "a -> empty" $ do
    diffTextsToChangeEvents "a" "" `shouldBe` [ChangeEvent (Range (Position 0 0) (Position 0 1)) ""]

  it "ab -> empty" $ do
    diffTextsToChangeEvents "ab" "" `shouldBe` [ChangeEvent (Range (Position 0 0) (Position 0 2)) ""]

  it "\\na -> \\n" $ do
    diffTextsToChangeEvents "\na" "\n" `shouldBe` [ChangeEvent (Range (Position 1 0) (Position 1 1)) ""]

  it "\\n\\na -> \\n\\n" $ do
    diffTextsToChangeEvents "\n\na" "\n\n" `shouldBe` [ChangeEvent (Range (Position 2 0) (Position 2 1)) ""]

  -- describe "Single-line cases" $ do
  --   it "simple insertion" $ do
  --     liftIO (diffTextsToChangeEvents "ab" "abc") >>= (`shouldBe` ([ChangeEvent (Range (Position 0 2) (Position 0 2)) "c"]))

  --   it "simple deletion" $ do
  --     liftIO (diffTextsToChangeEvents "abc" "ab") >>= (`shouldBe` ([ChangeEvent (Range (Position 0 2) (Position 0 3)) ""]))

  -- describe "QuickCheck" $ introduceQuickCheck $ modifyMaxSuccess (const 10000) $ do
  --   prop "Single change" $ \(InsertOrDelete (from, to)) -> verifyDiff from to
  --   prop "Multiple changes" $ \(MultiInsertOrDelete (from, to)) -> verifyDiff from to

file1 :: Text
file1 =
  [__i|foo = 42
       :t foo

       homophones <- readFile "homophones.list"

       putStrLn "HI"

       abc

       import Data.Aeson as A

       -- | Here's a nice comment on bar
       bar :: IO ()
       bar = do
         putStrLn "hello"
         putStrLn "world"
      |]

file2 :: Text
file2 =
  [__i|foo = 42
       :t foo

       homophones <- readFile "homophones.list"

       putStrLn "HI"

       a

       import Data.Aeson as A

       -- | Here's a nice comment on bar
       bar :: IO ()
       bar = do
         putStrLn "hello"
         putStrLn "world"
      |]



verifyDiff from to = applyChangesText change from == to
  where change = diffTextsToChangeEvents from to

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
