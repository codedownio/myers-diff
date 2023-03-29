{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.DeepSeq
import Criterion
import Criterion.Main
import Data.Diff.Types
import GHC.Generics

import qualified Data.Diff.DiffMyersShim as DM


deriving instance Generic Position
deriving instance NFData Position

deriving instance Generic Range
deriving instance NFData Range

deriving instance Generic ChangeEvent
deriving instance NFData ChangeEvent

getPair :: IO (String, String)
getPair = do
  putStrLn "Generating pair"
  return ("abc", "abcd")

main :: IO ()
main = defaultMain [
  env getPair $ \(~(initial, final)) -> do
    bench "Simple" $ nf (\(x, y) -> DM.diffDiff x y) (initial, final)
  ]
