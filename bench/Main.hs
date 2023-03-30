{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.DeepSeq
import Criterion
import Criterion.Main
import Data.Diff.Types
import Data.String.Interpolate
import Data.Text as T
import GHC.Generics

import qualified Data.Diff.DiffMyersShim as DM
import qualified Data.Diff.VectorMyers as VM


deriving instance Generic Position
deriving instance NFData Position

deriving instance Generic Range
deriving instance NFData Range

deriving instance Generic ChangeEvent
deriving instance NFData ChangeEvent

getPair :: IO (String, String, Text, Text)
getPair = do
  putStrLn "Generating pair"
  return (T.unpack file1, T.unpack file2, file1, file2)

main :: IO ()
main = defaultMain [
  env getPair $ \(~(initial, final, initialText, finalText)) ->
    bgroup "Simple" [
      bench "Diff" $ nf (\(x, y) -> DM.diffDiff x y) (initial, final)
      , bench "Vector" $ nf (\(x, y) -> VM.diffTextsToChangeEvents x y) (initialText, finalText)
    ]
  ]


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
