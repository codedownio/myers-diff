{-# LANGUAGE CPP #-}

module Main (main) where

import qualified Data.Diff.Myers as VM
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import TestLib.Benchmarking
import TestLib.Instances ()
import Weigh

#ifdef DIFF
import qualified Data.Diff.Diff as DD
#endif


testFunc :: [(String, String, Text, Text)] -> Weigh ()
testFunc samples = do
  func' "myers-diff" (L.map (\(_, _, initialText, finalText) -> (VM.diffTexts initialText finalText))) samples
#ifdef DIFF
  func' "Diff" (L.map (\(initialString, finalString, _, _) -> (DD.diff initialString finalString))) samples
#endif

main :: IO ()
main = do
  samples10 <- getPairSingleInsert 100 10
  samples100 <- getPairSingleInsert 100 100
  samples1000 <- getPairSingleInsert 100 1000
  samples10000 <- getPairSingleInsert 100 10000
  samples100000 <- getPairSingleInsert 100 100000

  mainWith $ do
    wgroup [i|Single insert (100 samples each)|] $ do
      testFunc samples10
      testFunc samples100
      testFunc samples1000
      testFunc samples10000
      testFunc samples100000

    -- , wgroup [i|Single delete (100 samples each)|] [
    --            testFunc getPairSingleDelete 100 10
    --            , testFunc getPairSingleDelete 100 100
    --            , testFunc getPairSingleDelete 100 1000
    --            , testFunc getPairSingleDelete 100 10000
    --            -- , testFunc getPairSingleDelete 100 100000
    --            ]
