{-# LANGUAGE DataKinds #-}
module SPL.Parser.Program where

import SPL.AST
import SPL.Parser.Parser (Parser)
import SPL.Parser.Decl (pDecl)
import qualified SPL.Parser.Lexer as L

import Control.Applicative (some)

-- Parses any program.
-- Grammar: decl+
pProgram :: Parser (Program ParsedP)
pProgram = L.whitespace *> some pDecl
