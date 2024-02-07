{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Parser.Lexer where

import Parser.Parser (Parser)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec (between, many, (<|>))

-- https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec-Char-Lexer.html#v:space
whitespace = L.space
  space1                         -- skip whitespace, newlines and tabs
  (L.skipLineComment "//")       -- skip line comments (starting with "//")
  (L.skipBlockComment "/*" "*/") -- skip block comments (starting with "/*" and ending with "*/")

-- https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec-Char-Lexer.html#v:lexeme
lexeme = L.lexeme whitespace

-- https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec-Char-Lexer.html#v:symbol
symbol = L.symbol whitespace
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")

tIdentifier :: Parser T.Text
tIdentifier = do
  first <- letterChar <|> char '_'
  middle <- many (alphaNumChar <|> char '_')
  apostrophes <- many $ char '\''
  return $ T.pack $ first : (middle ++ apostrophes)

-- https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec-Char-Lexer.html#g:3
tChar :: Parser Char
tChar = char '\'' *> L.charLiteral <* char '\''

-- https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec-Char-Lexer.html#v:decimal
tInteger :: Parser Int
tInteger = L.signed whitespace L.decimal

-- https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec-Char-Lexer.html#v:float
tFloat :: Parser Float
tFloat = L.signed whitespace L.float

tComma :: Parser T.Text
tComma = symbol ","

tTrue :: Parser T.Text
tTrue = symbol "true"

tFalse :: Parser T.Text
tFalse = symbol "false"

tEmptyList :: Parser T.Text
tEmptyList = symbol "[]"
