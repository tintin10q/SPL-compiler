module Parser.Program where

import Parser.AST
import Parser.Parser (Parser)
import Control.Applicative (some)
import Parser.Decl (pDecl)

-- Parses any program.
-- Grammar: decl+
pProgram :: Parser Program
pProgram = some pDecl
