{-# LANGUAGE CPP #-}

module Main (main) where

import Criterion
import Criterion.Main
import qualified Data.Diff.VectorMyers as VM
import Data.String.Interpolate
import Data.Text as T
import TestLib.Instances ()

#ifdef DIFF_MYERS
import qualified Data.Diff.DiffMyersShim as DM
#endif


getPair :: IO (String, String, Text, Text)
getPair = do
  putStrLn "Generating pair"
  return (T.unpack file1, T.unpack file2, file1, file2)

main :: IO ()
main = defaultMain [
  env getPair $ \(~(initial, final, initialText, finalText)) ->
    bgroup "Simple" [
      bench "Vector to edit script" $ nf (\(x, y) -> VM.diffTexts x y) (initialText, finalText)
      , bench "Vector to edit script (consolidated)" $ nf (\(x, y) -> VM.consolidateEditScript $ VM.diffTexts x y) (initialText, finalText)
      , bench "Vector to ChangeEvent" $ nf (\(x, y) -> VM.diffTextsToChangeEvents x y) (initialText, finalText)
      , bench "Vector to ChangeEvents (consolidated)" $ nf (\(x, y) -> VM.diffTextsToChangeEventsConsolidate x y) (initialText, finalText)

#ifdef DIFF_MYERS
      , bench "Diff" $ nf (\(x, y) -> DM.diffDiff x y) (initial, final)
#endif
    ]
  ]


file1 :: Text
file1 =
  [__i|foo = 42
       :t foo

       homophones <- readFile "homophones.list"

       putStrLn "HI"

       abc

       import Data.Aeson as A

       -- | Here's a nice comment on bar
       bar :: IO ()
       bar = do
         putStrLn "hello"
         putStrLn "world"
      |]

file2 :: Text
file2 =
  [__i|foo = 42
       :t foo

       homophones <- readFile "homophones.list"

       putStrLn "HI"

       a

       import Data.Aeson as A

       -- | Here's a nice comment on bar
       bar :: IO ()
       bar = do
         putStrLn "hello"
         putStrLn "world"
      |]
