
module Main (main) where

import Criterion
import Criterion.Main
import Data.String.Interpolate
import TestLib.Benchmarking

main :: IO ()
main = defaultMain [
  bgroup [i|Small insert|] [
             testGroup getPairSingleInsert 100 10
             , testGroup getPairSingleInsert 100 100
             , testGroup getPairSingleInsert 100 1000
             , testGroup getPairSingleInsert 100 10000
             , testGroup getPairSingleInsert 100 100000
             ]
  ]
