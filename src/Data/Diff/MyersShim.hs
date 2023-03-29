
module Data.Diff.MyersShim (
  editScriptToChangeEvents
  ) where

import Data.Diff.Types
import Data.Sequence
import Data.Vector.Unboxed as VU
import Prelude hiding (read)


editScriptToChangeEvents :: VU.Vector a -> VU.Vector a -> Seq Edit -> Seq ChangeEvent
editScriptToChangeEvents left right = go mempty 0
  where
    go :: Seq ChangeEvent -> Int -> Seq Edit -> Seq ChangeEvent
    go seqSoFar _pos Empty = seqSoFar

    go seqSoFar pos ((EditDelete from to) :<| rest) = go (seqSoFar |> newChange) newPos rest
      where
        newChange = ChangeEvent range ""
        range = undefined
        newPos = undefined

    go seqSoFar pos ((EditInsert at from to) :<| rest) = go (seqSoFar |> newChange) newPos rest
      where
        newChange = undefined
        newPos = undefined
