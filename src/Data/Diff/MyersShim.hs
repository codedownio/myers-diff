
module Data.Diff.MyersShim (
  editScriptToChangeEvents
  ) where

import Data.Diff.Types
import Data.Sequence
import Data.Vector.Unboxed as VU
import Prelude hiding (read)


editScriptToChangeEvents :: VU.Vector Char -> VU.Vector Char -> Seq Edit -> Seq ChangeEvent
editScriptToChangeEvents left right = go mempty 0 0
  where
    go :: Seq ChangeEvent -> Int -> Int -> Seq Edit -> Seq ChangeEvent
    go seqSoFar _curLine _curCh Empty = seqSoFar

    go seqSoFar curLine curCh ((EditDelete from to) :<| rest) = go (seqSoFar |> newChange) newLine newCh rest
      where
        newChange = ChangeEvent range ""
        range = undefined
        newLine = undefined
        newCh = undefined

    go seqSoFar curLine curCh ((EditInsert at from to) :<| rest) = go (seqSoFar |> newChange) newLine newCh rest
      where
        newChange = undefined
        newLine = undefined
        newCh = undefined
