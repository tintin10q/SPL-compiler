{-# LANGUAGE OverloadedStrings #-}
module Parser.Literal where

import Parser.AST (Literal (..))
import Parser.Parser (Parser)
import Text.Megaparsec.Char

pTrue :: Parser Literal
pTrue = TrueLit <$ string "true"

pFalse :: Parser Literal
pFalse = FalseLit <$ string "false"
