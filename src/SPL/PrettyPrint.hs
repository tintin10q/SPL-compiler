module SPL.PrettyPrint (prettyPrintMap, printWithCommas) where

import qualified Data.Map as Map
import Data.Map (Map)
import Text.PrettyPrint (render, vcat, text)

-- Pretty print function for Map
prettyPrintMap :: (Show k, Show v) => Map k v -> String
prettyPrintMap m = render $ vcat $ map formatEntry (Map.toList m)
  where
    formatEntry (k, v) = text (show k) <> text " -> " <> text (show v)



-- Function to print the list with commas, excluding the last element
printWithCommas :: [String] -> String
printWithCommas [] = ""
printWithCommas [x] = x
printWithCommas [x, lst] = x ++ " and " ++ lst
printWithCommas (x:xs) = x ++ (if null xs then "" else ", ") ++ printWithCommas xs


