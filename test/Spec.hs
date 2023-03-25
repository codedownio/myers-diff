{-# LANGUAGE CPP #-}

module Main where

import Test.Sandwich

#ifdef UNI_MYERS
import Spec.UniMyersSpec as UniMyersSpec
#endif

import Spec.DiffMyersSpec as DiffMyersSpec


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions $ do
#ifdef UNI_MYERS
  UniMyersSpec.spec
#endif

  DiffMyersSpec.spec
