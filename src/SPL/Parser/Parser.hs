module SPL.Parser.Parser (Parser) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text
--                   ^    ^
--                   |    |
-- Custom error component Type of input stream
-- Until we start dealing with custom parsing errors when you see Parser in the chapter, assume this type.
