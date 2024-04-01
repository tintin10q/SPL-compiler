{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module SPL.Parser.Parser (srcSpan, SourceSpan, Parser) where

import SPL.Parser.AST

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text
--                   ^    ^
--                   |    |
-- Custom error component Type of input stream
-- Until we start dealing with custom parsing errors when you see Parser in the chapter, assume this type.

newtype SourceSpan = SourceSpan (SourcePos, SourcePos)

srcSpan :: SourcePos -> SourcePos -> SourceSpan
srcSpan start end = SourceSpan (start, end)

type instance FunDecl ParsedP = SourceSpan
type instance VarDecl ParsedP = SourceSpan

type instance ReturnStmt ParsedP = SourceSpan
type instance IfStmt ParsedP = SourceSpan
type instance WhileStmt ParsedP = SourceSpan
type instance ExprStmt ParsedP = SourceSpan
type instance VarStmt ParsedP = SourceSpan

type instance BinOpExpr ParsedP = SourceSpan
type instance UnaryOpExpr ParsedP = SourceSpan
type instance AssignExpr ParsedP = SourceSpan
type instance FunctionCallExpr ParsedP = SourceSpan
type instance VariableExpr ParsedP = SourceSpan
type instance LiteralExpr ParsedP = SourceSpan
