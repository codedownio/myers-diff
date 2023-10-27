
module TestLib.Benchmarking (
  getPairSingleInsert
  , getPairSingleDelete

  , getPairWithEdit

  , PairFn
  ) where

import Control.Monad
import Data.Text as T
import Test.QuickCheck
import TestLib.Generators
import TestLib.Instances ()


type PairFn = Int -> Int -> IO [(String, String, Text, Text)]

getPairWithEdit :: (Text -> Gen Text) -> Int -> Int -> IO [(String, String, Text, Text)]
getPairWithEdit makeEdit numSamples inputSize = do
  replicateM numSamples $ do
    (t1, t2) <- generate ((resize inputSize arbitraryAlphanumericString) >>= (\initial -> (initial, ) <$> makeEdit initial))
    return (T.unpack t1, T.unpack t2, t1, t2)

getPairSingleInsert :: PairFn
getPairSingleInsert = getPairWithEdit arbitraryInsertOn

getPairSingleDelete :: PairFn
getPairSingleDelete = getPairWithEdit arbitraryDeleteOn
