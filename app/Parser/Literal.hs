{-# LANGUAGE OverloadedStrings #-}
module Parser.Literal where

import Parser.AST (Literal (..))
import Parser.Parser (Parser)
import Text.Megaparsec.Char

pTrue :: Parser Literal
pTrue = TrueLit <$ string "true"

pFalse :: Parser Literal
pFalse = FalseLit <$ string "false"

pInt :: Parser Literal
pInt = undefined

pFloat :: Parser Literal
pFloat = undefined

pChar :: Parser Literal
pChar = undefined

pTuple :: Parser Literal
pTuple = undefined

pEmptyList :: Parser Literal
pEmptyList = EmptyListLit <$ string "[]"