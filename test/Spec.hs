{-# LANGUAGE CPP #-}

module Main (main) where

import Test.Sandwich

#ifdef UNI_MYERS
import Spec.UniMyersSpec as UniMyersSpec
#endif

import Spec.DiffMyersSpec as DiffMyersSpec
import Spec.VectorMyersSpec as VectorMyersSpec


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions $ do
#ifdef UNI_MYERS
  UniMyersSpec.spec
#endif

  DiffMyersSpec.spec

  VectorMyersSpec.spec
