module Parser.Program where

import Parser.AST
import Parser.Parser (Parser)
import Control.Applicative (some)
import Parser.Decl (pDecl)
import qualified Parser.Lexer as L

-- Parses any program.
-- Grammar: decl+
pProgram :: Parser Program
pProgram = L.whitespace *> some pDecl
