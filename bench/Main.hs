
module Main (main) where

import Control.DeepSeq
import Criterion
import Criterion.Main
import GHC.Generics

import qualified Data.Diff.DiffMyersShim as DM


getPair :: IO (String, String)
getPair = do
  putStrLn "Generating pair"
  return ("abc", "abcd")

main :: IO ()
main = defaultMain [
  env getPair $ \(~(initial, final)) -> do
    bench "Simple" $ nf (\(x, y) -> DM.diffDiff x y) (initial, final)
  ]
