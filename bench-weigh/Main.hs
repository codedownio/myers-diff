{-# LANGUAGE CPP #-}

module Main (main) where

import qualified Data.Diff.Myers as VM
import Data.String
import Data.String.Interpolate
import Data.Text as T
import Data.Text.Encoding as T
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Mutable as VUM
import TestLib.Instances ()
import Weigh

#ifdef DIFF_MYERS
import qualified Data.Diff.DiffMyersShim as DM
#endif


main :: IO ()
main = mainWith $ do
#ifdef DIFF_MYERS
  func "Diff" (\(x, y) -> DM.diffDiff x y) (T.unpack file1, T.unpack file2)
#endif

  func "Vector" (\(x, y) -> VM.diffTextsToChangeEvents x y) (file1, file2)

  -- value "file1 vector" (force (VU.generate (T.length file1) (\i -> T.index file1 i)))
  -- value "file2 vector" (force (VU.generate (T.length file2) (\i -> T.index file2 i)))

  -- value "file1" file1
  -- value "file2" file2

  value "file2 string" (T.unpack file2)
  value "file2 bytes" (T.encodeUtf8 file2)

  action "file2 vector" (VUM.generate (T.length file2) (\i -> T.index file2 i))



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
