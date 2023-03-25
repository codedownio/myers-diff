
module TestLib.Apply where

import qualified Data.List as L
import Data.Text
import Data.Text.Rope (Rope)
import qualified Data.Text.Rope as Rope
import Language.LSP.Types as J


-- * Text

applyChangesText :: [J.TextDocumentContentChangeEvent] -> Text -> Text
applyChangesText changes rope = L.foldl' (flip applyChangeText) rope changes

applyChangeText :: J.TextDocumentContentChangeEvent -> Text -> Text
applyChangeText change t = Rope.toText (applyChange change (Rope.fromText t))

-- * Rope

applyChanges :: [J.TextDocumentContentChangeEvent] -> Rope -> Rope
applyChanges changes rope = L.foldl' (flip applyChange) rope changes

applyChange :: J.TextDocumentContentChangeEvent -> Rope -> Rope
applyChange (J.TextDocumentContentChangeEvent Nothing _ str) _
  = Rope.fromText str
applyChange (J.TextDocumentContentChangeEvent (Just (J.Range (J.Position sl sc) (J.Position fl fc))) _ txt) str
  = changeChars str (Rope.Position (fromIntegral sl) (fromIntegral sc)) (Rope.Position (fromIntegral fl) (fromIntegral fc)) txt

changeChars :: Rope -> Rope.Position -> Rope.Position -> Text -> Rope
changeChars str start finish new = mconcat [before', Rope.fromText new, after]
 where
   (before, after) = Rope.splitAtPosition finish str
   (before', _) = Rope.splitAtPosition start before
