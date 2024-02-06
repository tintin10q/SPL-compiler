{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Parser.Parser where

import Parser.AST
import Data.Void
import Text.Megaparsec
import Data.Text (Text)
import Text.Megaparsec.Char

type Parser = Parsec Void Text

pIdentifier :: Parser Identifier
pIdentifier = do
    identifier <- some (letterChar <|> single '_')
    apostrophes <- many (single '\'')
    return $ identifier ++ apostrophes
