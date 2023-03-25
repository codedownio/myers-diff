{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.DeepSeq
import Criterion
import Criterion.Main
import GHC.Generics
import Language.LSP.Types

import qualified Data.Diff.DiffMyersShim as DM


getPair :: IO (String, String)
getPair = do
  putStrLn "Generating pair"
  return ("abc", "abcd")

deriving instance Generic TextDocumentContentChangeEvent
deriving instance NFData TextDocumentContentChangeEvent

main :: IO ()
main = defaultMain [
  env getPair $ \(~(initial, final)) -> do
    bench "Simple" $ nf (\(x, y) -> DM.diffDiff x y) (initial, final)
  ]
