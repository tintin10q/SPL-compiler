{-# LANGUAGE OverloadedStrings #-}
module Parser.Literal where

import Parser.AST (Literal (..))
import Parser.Parser (Parser)
import Text.Megaparsec.Char
import qualified Parser.Lexer as L

pTrue :: Parser Literal
pTrue = TrueLit <$ L.true

pFalse :: Parser Literal
pFalse = FalseLit <$ L.false

pInt :: Parser Literal
pInt = IntLit <$> L.integer

pFloat :: Parser Literal
pFloat = FloatLit <$> L.float

pChar :: Parser Literal
pChar = CharLit <$> (char '\'' *> L.char <* char '\'')

pTuple :: Parser Literal
pTuple = undefined

pEmptyList :: Parser Literal
pEmptyList = EmptyListLit <$ string "[]"