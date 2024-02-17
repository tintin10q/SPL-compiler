module Utils where

optionList :: Maybe [a] -> [a]
optionList Nothing = []
optionList (Just xs) = xs