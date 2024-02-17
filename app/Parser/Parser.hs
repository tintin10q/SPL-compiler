module Parser.Parser where

import Data.Void
import Text.Megaparsec
import Data.Text (Text)

type Parser = Parsec Void Text
--                   ^    ^
--                   |    |
-- Custom error component Type of input stream
-- Until we start dealing with custom parsing errors when you see Parser in the chapter, assume this type.
