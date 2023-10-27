{-# LANGUAGE CPP #-}

module Main (main) where

import Criterion
import Criterion.Main
import qualified Data.Diff.Myers as VM
import qualified Data.List as L
import Data.String.Interpolate
import TestLib.Benchmarking

#ifdef DIFF
import qualified Data.Diff.Diff as DD
#endif


testGroup :: PairFn -> Int -> Int -> Benchmark
testGroup getPair numSamples inputSize =
  env (getPair numSamples inputSize) $ \samples -> do
    bgroup [i|#{inputSize} characters|] [
      bench "myers-diff" $ nf (L.map (\(_, _, initialText, finalText) -> (VM.diffTexts initialText finalText))) samples
#ifdef DIFF
      , bench "Diff" $ nf (L.map (\(initialString, finalString, _, _) -> (DD.diff initialString finalString))) samples
#endif
      ]

main :: IO ()
main = defaultMain [
  bgroup [i|Single insert (100 samples each)|] [
             testGroup getPairSingleInsert 100 10
             , testGroup getPairSingleInsert 100 100
             , testGroup getPairSingleInsert 100 1000
             , testGroup getPairSingleInsert 100 10000
             , testGroup getPairSingleInsert 100 100000
             ]
  , bgroup [i|Single delete (100 samples each)|] [
             testGroup getPairSingleDelete 100 10
             , testGroup getPairSingleDelete 100 100
             , testGroup getPairSingleDelete 100 1000
             , testGroup getPairSingleDelete 100 10000
             -- , testGroup getPairSingleDelete 100 100000
             ]
  ]
