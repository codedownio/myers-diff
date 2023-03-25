
module Data.Diff.DiffMyersShim (
  diffDiff
  ) where

import Data.Function
import qualified Data.List as L
import qualified Data.Text as T
import Language.LSP.Types

import qualified Data.Algorithm.Diff as DD


utilDiffToLspDiff :: [DD.Diff String] -> [TextDocumentContentChangeEvent]
utilDiffToLspDiff elems = go [] 0 0 elems
  where
    go events curLine curChar ((DD.Both chars _):xs) = go events curLine' curChar' xs
      where
        curLine' = curLine + countNewlines chars
        curChar' = if hasNewline chars then fromIntegral (lengthOfLastLine chars) else curChar + fromIntegral (L.length chars)

    go events curLine curChar ((DD.First chars):xs) = go ((TextDocumentContentChangeEvent (Just (Range startPos endPos)) Nothing ""):events) curLine' curChar' xs
      where
        startPos = Position curLine curChar
        endPos = Position (curLine + countNewlines chars) (if hasNewline chars then fromIntegral (lengthOfLastLine chars) else curChar + fromIntegral (L.length chars))

        curLine' = curLine
        curChar' = curChar

    go events curLine curChar ((DD.Second chars):xs) = go ((TextDocumentContentChangeEvent (Just (Range startPos endPos)) Nothing (T.pack chars)):events) curLine' curChar' xs
      where
        startPos = Position curLine curChar
        endPos = startPos

        curLine' = curLine + countNewlines chars
        curChar' = if hasNewline chars then fromIntegral (lengthOfLastLine chars) else curChar + fromIntegral (L.length chars)

    go events _curLine _curChar [] = reverse events

    hasNewline = any (== '\n')

    lengthOfLastLine chars = chars
                           & T.pack
                           & T.splitOn "\n"
                           & last
                           & T.length

    countNewlines = L.foldl' (\total c -> if c == '\n' then total + 1 else total) 0

diffDiff :: String -> String -> [TextDocumentContentChangeEvent]
diffDiff s1 s2 = utilDiffToLspDiff (DD.getGroupedDiff s1 s2)
