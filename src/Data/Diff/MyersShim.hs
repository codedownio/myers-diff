
module Data.Diff.MyersShim (
  editScriptToChangeEvents
  ) where

import Data.Diff.Types
import Data.Function
import Data.Sequence
import Data.Vector.Unboxed as VU
import Prelude hiding (read)


editScriptToChangeEvents :: VU.Vector Char -> VU.Vector Char -> Seq Edit -> Seq ChangeEvent
editScriptToChangeEvents left right = go mempty 0 0 0 0
  where
    go :: Seq ChangeEvent -> Int -> Int -> Int -> Int -> Seq Edit -> Seq ChangeEvent
    go seqSoFar _ _ _ _ Empty = seqSoFar

    go seqSoFar lastPos curLine curCh offset ((EditDelete ((\x -> x - offset) -> from) ((\x -> x - offset) -> to)) :<| rest) = go (seqSoFar |> newChange) newPos newLine newCh newOffset rest
      where
        newChange = ChangeEvent range ""
        range = Range (Position l1 c1) (Position l2 c2)

        unchangedNewlines = countNewlines (VU.slice lastPos (from - lastPos) left)

        l1 = curLine + unchangedNewlines
        c1 = if | unchangedNewlines == 0 -> curCh + (from - lastPos)
                | otherwise -> lengthOfLastLine (VU.slice lastPos (from - lastPos) left)

        deleted = VU.slice from (to - from) left
        numNewlinesInDeleted = countNewlines deleted

        l2 = l1 + numNewlinesInDeleted
        c2 = if | numNewlinesInDeleted == 0 -> curCh + (to + 1 - lastPos)
                | otherwise -> 43

        newPos = lastPos
        newLine = curLine
        newCh = curCh
        newOffset = offset + to - from + 1

    go seqSoFar lastPos curLine curCh offset ((EditInsert at from to) :<| rest) = go (seqSoFar |> newChange) newPos newLine newCh newOffset rest
      where
        newOffset = undefined
        newChange = undefined
        newPos = undefined
        newLine = undefined
        newCh = undefined



countNewlines :: VU.Vector Char -> Int
countNewlines = foldl' (\tot ch -> if ch == '\n' then tot + 1 else tot) 0

lengthOfLastLine :: Vector Char -> Int
lengthOfLastLine vec = flip fix (0, VU.length vec - 1) $ \loop -> \case
  (tot, i)
    | i < 0 -> tot
    | vec `unsafeIndex` i == '\n' -> tot
    | otherwise -> loop (tot + 1, i - 1)
