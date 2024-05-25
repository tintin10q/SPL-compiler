module SPL.PrettyPrint (prettyPrintMap) where

import qualified Data.Map as Map
import Data.Map (Map)
import Text.PrettyPrint (render, vcat, text)

-- Pretty print function for Map
prettyPrintMap :: (Show k, Show v) => Map k v -> String
prettyPrintMap m = render $ vcat $ map formatEntry (Map.toList m)
  where
    formatEntry (k, v) = text (show k) <> text " -> " <> text (show v)