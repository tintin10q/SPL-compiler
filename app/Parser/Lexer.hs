{-# LANGUAGE OverloadedStrings #-}
module Parser.Lexer where

import Parser.Parser (Parser)
import Text.Megaparsec.Char
import Data.Text (Text)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec (between)

-- https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec-Char-Lexer.html#v:space
spaceConsumer :: Parser ()
spaceConsumer = L.space
  space1                         -- skip whitespace, newlines and tabs
  (L.skipLineComment "//")       -- skip line comments (starting with "//")
  (L.skipBlockComment "/*" "*/") -- skip block comments (starting with "/*" and ending with "*/")

-- https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec-Char-Lexer.html#v:lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec-Char-Lexer.html#v:symbol
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser Text -> Parser Text
parens = between (symbol "(") (symbol ")")

braces :: Parser Text -> Parser Text
braces = between (symbol "{") (symbol "}")

brackets :: Parser Text -> Parser Text
brackets = between (symbol "[") (symbol "]")

semicolon :: Parser Text
semicolon = symbol ";"

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

dot :: Parser Text
dot = symbol "."

true :: Parser Text
true = symbol "true"

false :: Parser Text
false = symbol "false"

-- https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec-Char-Lexer.html#g:3
char :: Parser Char
char = L.charLiteral

-- https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec-Char-Lexer.html#v:decimal
integer :: Parser Int
integer = L.signed spaceConsumer L.decimal

-- https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec-Char-Lexer.html#v:float
float :: Parser Float
float = L.signed spaceConsumer L.float


