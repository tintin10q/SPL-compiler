module Parser.Parser where

import Data.Void
import Text.Megaparsec
import Data.Text (Text)

type Parser = Parsec Void Text
