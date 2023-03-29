
module Data.Diff.MyersShim (
  editScriptToChangeEvents
  ) where

import Data.Diff.Types
import Data.Function
import Data.Sequence
import qualified Data.Text as T
import Data.Vector.Unboxed as VU
import Prelude hiding (read)

import Data.String.Interpolate
import Debug.Trace


editScriptToChangeEvents :: VU.Vector Char -> VU.Vector Char -> Seq Edit -> Seq ChangeEvent
editScriptToChangeEvents left right = go mempty 0 0 0
  where
    go :: Seq ChangeEvent -> Int -> Int -> Int -> Seq Edit -> Seq ChangeEvent
    go seqSoFar _ _ _ Empty = seqSoFar

    -- Implicit unchanged section before delete
    go seqSoFar pos line ch args@((EditDelete from to) :<| _) |
      pos < from = trace [i|CONSUMED from #{pos} to #{from}.\nRecursing with #{from} (#{line'}, #{ch'}) #{args}\n|] $ go seqSoFar from line' ch' args
        where
          (numNewlinesEncountered, lastLineLength) = countNewlinesAndLastLineLength (VU.slice pos (from - pos) left)
          line' = line + numNewlinesEncountered
          ch' | numNewlinesEncountered == 0 = ch + (from - pos)
              | otherwise = lastLineLength
    -- Implicit unchanged section before insert
    go seqSoFar pos line ch args@((EditInsert from rightFrom rightTo) :<| _) |
      pos < from = go seqSoFar from line' ch' args
        where
          (numNewlinesEncountered, lastLineLength) = countNewlinesAndLastLineLength (VU.slice pos (from - pos) left)
          line' = line + numNewlinesEncountered
          ch' | numNewlinesEncountered == 0 = ch + (from - pos)
              | otherwise = lastLineLength

    go seqSoFar pos line ch ((EditDelete from to) :<| rest) = trace [i|DELETE #{change}.\nRecursing with #{pos'} (#{line'}, #{ch'}) #{rest}\n|] go (seqSoFar |> change) pos' line ch rest
      where
        change = ChangeEvent (Range (Position line ch) (Position line' ch')) ""
        pos' = to + 1

        deleted = VU.slice from (to + 1 - from) left
        (numNewlinesInDeleted, lastLineLengthInDeleted) = countNewlinesAndLastLineLength deleted
        line' = line + numNewlinesInDeleted
        ch' = if | numNewlinesInDeleted == 0 -> ch + (to - pos + 1)
                 | otherwise -> lastLineLengthInDeleted

    go seqSoFar pos line ch ((EditInsert at rightFrom rightTo) :<| rest) = trace [i|INSERT #{change}.\nRecursing with #{pos'} (#{line'}, #{ch'}) #{rest}\n|] $ go (seqSoFar |> change) pos' line' ch' rest
      where
        change = ChangeEvent (Range (Position line ch) (Position line ch)) (vectorToText inserted)
        pos' = pos

        inserted = VU.slice rightFrom (rightTo + 1 - rightFrom) right
        (numNewlinesInInserted, lastLineLengthInInserted) = countNewlinesAndLastLineLength (trace [i|Got inserted: #{inserted}|] inserted)
        line' = line + numNewlinesInInserted
        ch' = if | numNewlinesInInserted == 0 -> ch + VU.length inserted
                 | otherwise -> lastLineLengthInInserted



-- countNewlines :: VU.Vector Char -> Int
-- countNewlines = foldl' (\tot ch -> if ch == '\n' then tot + 1 else tot) 0

countNewlinesAndLastLineLength :: VU.Vector Char -> (Int, Int)
countNewlinesAndLastLineLength = foldl' (\(tot, lastLineLength) ch -> if ch == '\n' then (tot + 1, 0) else (tot, lastLineLength + 1)) (0, 0)

vectorToText :: VU.Vector Char -> T.Text
vectorToText = T.pack . VU.toList

-- lengthOfLastLine :: Vector Char -> Int
-- lengthOfLastLine vec = flip fix (0, VU.length vec - 1) $ \loop -> \case
--   (tot, i)
--     | i < 0 -> tot
--     | vec `unsafeIndex` i == '\n' -> tot
--     | otherwise -> loop (tot + 1, i - 1)
