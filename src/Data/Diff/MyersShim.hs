
module Data.Diff.MyersShim (
  editScriptToChangeEvents
  ) where

import Data.Diff.Types
import Data.Sequence
import Data.Vector.Unboxed as VU
import Prelude hiding (read)


editScriptToChangeEvents :: VU.Vector Char -> VU.Vector Char -> Seq Edit -> Seq ChangeEvent
editScriptToChangeEvents left right = go mempty 0 0 0
  where
    go :: Seq ChangeEvent -> Int -> Int -> Int -> Seq Edit -> Seq ChangeEvent
    go seqSoFar _lastPos _curLine _curCh Empty = seqSoFar

    go seqSoFar lastPos curLine curCh ((EditDelete from to) :<| rest) = go (seqSoFar |> newChange) newPos newLine newCh rest
      where
        newChange = ChangeEvent range ""
        range = Range (Position l1 c1) (Position l2 c2)

        unchangedNewlines = countNewlines (VU.slice lastPos from left)

        l1 = curLine + unchangedNewlines
        c1 = if | unchangedNewlines == 0 -> curCh + (from - lastPos)
                | otherwise -> undefined -- then lengthOfLastNewlineInRange vec lastPos from

        l2 = l1 + countNewlines (VU.slice from to left)
        c2 = undefined

        newPos = lastPos
        newLine = curLine
        newCh = curCh

    go seqSoFar lastPos curLine curCh ((EditInsert at from to) :<| rest) = go (seqSoFar |> newChange) newPos newLine newCh rest
      where
        newChange = undefined
        newPos = undefined
        newLine = undefined
        newCh = undefined



countNewlines :: VU.Vector Char -> Int
countNewlines = foldl' (\tot ch -> if ch == '\n' then tot + 1 else tot) 0
