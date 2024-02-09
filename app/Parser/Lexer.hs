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

tExcl :: Parser T.Text
tExcl = symbol "!"

tGt :: Parser T.Text
tGt = symbol ">"

tGte :: Parser T.Text
tGte = symbol ">="

tLt :: Parser T.Text
tLt = symbol "<"

tLte :: Parser T.Text
tLte = symbol "<="

tDoubleEq :: Parser T.Text
tDoubleEq = symbol "=="

tExclEq :: Parser T.Text
tExclEq = symbol "!="

tDoubleAmpersand :: Parser T.Text
tDoubleAmpersand = symbol "&&"

tDoublePipe :: Parser T.Text
tDoublePipe = symbol "||"

tStar :: Parser T.Text
tStar = symbol "*"

tPlus :: Parser T.Text
tPlus = symbol "+"

tMin :: Parser T.Text
tMin = symbol "-"

tPercent :: Parser T.Text
tPercent = symbol "%"

tSlash :: Parser T.Text
tSlash = symbol "/"

tColon :: Parser T.Text
tColon = symbol ":"

tSemiColon :: Parser T.Text
tSemiColon = symbol ";"

tDot :: Parser T.Text
tDot = symbol "."

tEq :: Parser T.Text
tEq = symbol "="

tTrue :: Parser T.Text
tTrue = symbol "true"

tFalse :: Parser T.Text
tFalse = symbol "false"

tEmptyList :: Parser T.Text
tEmptyList = symbol "[]"

-- Keywords:
tReturn :: Parser T.Text
tReturn = symbol "return"

tIf :: Parser T.Text
tIf = symbol "if"

tElse :: Parser T.Text
tElse = symbol "else"

tWhile :: Parser T.Text
tWhile = symbol "while"

tFor :: Parser T.Text
tFor = symbol "for"

tVar :: Parser T.Text
tVar = symbol "var"

-- Type types

tIntType :: Parser T.Text
tIntType = symbol "Int"

tCharType :: Parser T.Text
tCharType = symbol "Char"

tBoolType :: Parser T.Text
tBoolType = symbol "Bool"

tVoidType :: Parser T.Text
tVoidType = symbol "Void"
