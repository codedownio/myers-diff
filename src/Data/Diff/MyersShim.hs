
module Data.Diff.MyersShim (
  editScriptToChangeEvents
  ) where

import Data.Diff.Types
import Data.Sequence
import Prelude hiding (read)


editScriptToChangeEvents :: Seq Edit -> [ChangeEvent]
editScriptToChangeEvents = undefined
