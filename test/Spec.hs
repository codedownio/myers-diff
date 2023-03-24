{-# LANGUAGE CPP #-}

module Main where

import Test.Sandwich

#ifdef UNI_MYERS
import Spec.UniMyersSpec as UniMyersSpec
#endif


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions $ do
  UniMyersSpec.spec
