{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module TestLib.Generators where

import Data.Function
import Data.String.Interpolate
import Data.Text as T
import Test.QuickCheck as Q
import Test.QuickCheck.Instances.Text ()


newtype InsertOrDelete = InsertOrDelete (Text, Text)
  deriving (Show, Eq)
instance Arbitrary InsertOrDelete where
  arbitrary = InsertOrDelete <$> oneof [arbitraryInsert, arbitraryDelete]

newtype MultiInsertOrDelete = MultiInsertOrDelete (Text, Text)
  deriving (Show, Eq)
instance Arbitrary MultiInsertOrDelete where
  arbitrary = do
    initial <- arbitrary

    sized $ \n -> flip fix (n, initial) $ \loop -> \case
      (0, x) -> return (MultiInsertOrDelete (initial, x))
      (i, cur) -> do
        next <- oneof [arbitraryInsertOn cur, arbitraryDeleteOn cur]
        loop (i - 1, next)

-- * Gen

arbitraryInsert :: Gen (Text, Text)
arbitraryInsert = arbitrary >>= (\initial -> (initial, ) <$> arbitraryInsertOn initial)

arbitraryInsertOn :: Text -> Gen Text
arbitraryInsertOn initial = do
  toInsert <- arbitrary

  pos <- chooseInt (0, T.length initial)
  let (x, y) = T.splitAt pos initial

  return (x <> toInsert <> y)


arbitraryDelete :: Gen (Text, Text)
arbitraryDelete = arbitrary >>= (\initial -> (initial, ) <$> arbitraryDeleteOn initial)

arbitraryDeleteOn :: Text -> Gen Text
arbitraryDeleteOn initial = do
  pos1 <- chooseInt (0, max 0 (T.length initial - 1))
  pos2 <- chooseInt (pos1, T.length initial)

  let (x, y) = T.splitAt pos1 initial
  let (_, z) = T.splitAt (pos2 - pos1) y

  return (x <> z)

-- * Docs

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
