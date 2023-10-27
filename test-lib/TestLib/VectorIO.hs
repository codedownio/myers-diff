{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module TestLib.VectorIO where

import Data.Diff.Myers
import Data.Diff.Types
import qualified Data.Foldable as F
import Data.Sequence
import Data.Text as T
import Data.Vector.Unboxed as VU


-- * IO versions of the 'Data.Diff.VectorMyers' diff functions for benchmarking

-- | Diff 'Text's to produce an edit script.
diffTextsIO :: Text -> Text -> IO (Seq Edit)
diffTextsIO left right = do
  let l = VU.fromList (T.unpack left)
  let r = VU.fromList (T.unpack right)
  diff l r

-- | Diff 'Text's to produce LSP-style change events.
diffTextsToChangeEventsIO :: Text -> Text -> IO [ChangeEvent]
diffTextsToChangeEventsIO = diffTextsToChangeEventsIO' id

-- | Diff 'Text's to produce consolidated LSP-style change events.
diffTextsToChangeEventsIOConsolidate :: Text -> Text -> IO [ChangeEvent]
diffTextsToChangeEventsIOConsolidate = diffTextsToChangeEventsIO' consolidateEditScript

diffTextsToChangeEventsIO' :: (Seq Edit -> Seq Edit) -> Text -> Text -> IO [ChangeEvent]
diffTextsToChangeEventsIO' consolidateFn left right = do
  -- This is faster than VU.fromList (T.unpack left), right?
  let l = VU.fromList (T.unpack left)
  let r = VU.fromList (T.unpack right)
  edits <- diff l r
  return $ F.toList $ editScriptToChangeEvents l r (consolidateFn edits)

-- | To use in benchmarking against other libraries that use String
diffStringsIO :: String -> String -> IO (Seq Edit)
diffStringsIO left right = do
  let leftThawed = VU.fromList left
  let rightThawed = VU.fromList right
  diff leftThawed rightThawed
