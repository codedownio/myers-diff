{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.DeepSeq
import Criterion
import Criterion.Main
import Data.Diff.Types
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
  return ("abc", "abcd", T.pack "abc", T.pack "abcd")

main :: IO ()
main = defaultMain [
  env getPair $ \(~(initial, final, initialText, finalText)) ->
    bgroup "Simple" [
      bench "Diff" $ nf (\(x, y) -> DM.diffDiff x y) (initial, final)
      , bench "Vector" $ nf (\(x, y) -> VM.diffTextsToChangeEvents x y) (initialText, finalText)
    ]
  ]
