
module Main (main) where

import Data.Diff.VectorMyers
import Data.String.Interpolate
import Data.Text.IO as T
import Test.QuickCheck
import TestLib.Generators


main :: IO ()
main = do
  (doc, doc') <- generate $ variant (42 :: Int) $ resize 50 $ (arbitraryDoc >>= arbitraryChangesSized)
  T.putStrLn [i|Got doc: #{doc}|]
  T.putStrLn [i|Got doc': #{doc'}|]
  let diffs = consolidateEditScript $ diffTexts doc doc'
  T.putStrLn [i|Edit script: #{diffs}|]
