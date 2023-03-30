{-# LANGUAGE CPP #-}

module Main (main) where

import Test.Sandwich

#ifdef UNI_MYERS
import Spec.UniMyersSpec as UniMyersSpec
#endif

#ifdef DIFF_MYERS
import Spec.DiffMyersSpec as DiffMyersSpec
#endif

import Spec.VectorMyersSpec as VectorMyersSpec


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions $ do
#ifdef UNI_MYERS
  UniMyersSpec.spec
#endif

#ifdef DIFF_MYERS
  DiffMyersSpec.spec
#endif

  VectorMyersSpec.spec
