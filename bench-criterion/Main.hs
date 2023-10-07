{-# LANGUAGE CPP #-}

module Main (main) where

import Criterion
import Criterion.Main
import qualified Data.Diff.Myers as VM
import Data.String.Interpolate
import Data.Text as T
import Test.QuickCheck
import TestLib.Generators (arbitraryDocInsert)
import TestLib.Instances ()

#ifdef DIFF
import qualified Data.Diff.Diff as DD
#endif


getPair :: IO (String, String, Text, Text)
getPair = do
  putStrLn "Generating pair"
  (t1, t2) <- generate arbitraryDocInsert
  putStrLn [i|#{t1} vs #{t2}|]
  return (T.unpack t1, T.unpack t2, t1, t2)

main :: IO ()
main = defaultMain [
  env getPair $ \(~(initial, final, initialText, finalText)) ->
    bgroup "Diff" [
      bench "myers-diff" $ nf (uncurry VM.diffTexts) (initialText, finalText)
      -- , bench "Vector to edit script (consolidated)" $ nf (\(x, y) -> VM.consolidateEditScript $ VM.diffTexts x y) (initialText, finalText)
      -- , bench "Vector to ChangeEvent" $ nf (\(x, y) -> VM.diffTextsToChangeEvents x y) (initialText, finalText)
      -- , bench "Vector to ChangeEvents (consolidated)" $ nf (\(x, y) -> VM.diffTextsToChangeEventsConsolidate x y) (initialText, finalText)

#ifdef DIFF
      , bench "Diff" $ nf (\(x, y) -> DD.diff x y) (initial, final)
#endif
    ]
  ]
