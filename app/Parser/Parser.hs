module Parser.Parser where

import Parser.AST
import Text.Parsec.Text (Parser)
import Text.Parsec

program :: Parser Program
program = undefined

identifier :: Parser Identifier
identifier = do
    identifier' <- many1 letter
    apostrophes <- many (char '\'')
    return $ identifier' ++ apostrophes