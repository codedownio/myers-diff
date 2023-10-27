
module Main (main) where

import Criterion
import Criterion.Main
import Data.String.Interpolate
import TestLib.Benchmarking


main :: IO ()
main = defaultMain [
  bgroup [i|Delete <= 30 chars (100 samples each)|] [
             testGroup getPairSingleDelete 100 10
             , testGroup getPairSingleDelete 100 100
             , testGroup getPairSingleDelete 100 1000
             , testGroup getPairSingleDelete 100 10000
             , testGroup getPairSingleDelete 100 100000
             ]
  ]
