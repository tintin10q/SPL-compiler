{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module SPL.Parser.Parser (srcSpan, Parser) where

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

type instance FunDecl ParserP = SourceSpan

type instance ReturnStmt ParserP = SourceSpan
type instance IfStmt ParserP = SourceSpan
type instance WhileStmt ParserP = SourceSpan
type instance ExprStmt ParserP = SourceSpan
type instance VarStmt ParserP = SourceSpan

type instance BinOpExpr ParserP = SourceSpan
type instance UnaryOpExpr ParserP = SourceSpan
type instance AssignExpr ParserP = SourceSpan
type instance FunctionCallExpr ParserP = SourceSpan
type instance VariableExpr ParserP = SourceSpan
type instance LiteralExpr ParserP = SourceSpan

type instance TrueLit ParserP = SourceSpan
type instance FalseLit ParserP = SourceSpan
type instance IntLit ParserP = SourceSpan
type instance FloatLit ParserP = SourceSpan
type instance CharLit ParserP = SourceSpan
type instance TupleLit ParserP = SourceSpan
type instance EmptyListLit ParserP = SourceSpan

type instance Identifier ParserP = SourceSpan
