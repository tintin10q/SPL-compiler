{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- todo maybe we can remove endPos and startPos exports
module SPL.Parser.Parser (srcSpan, SourceSpan, Parser, show, startPos, endPos, showStart, showEnd) where

import SPL.AST

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text
--                   ^    ^
--                   |    |
-- Custom error component Type of input stream
-- Until we start dealing with custom parsing errors when you see Parser in the chapter, assume this type.

newtype SourceSpan = SourceSpan (SourcePos, SourcePos)
  deriving (Eq)

startPos :: SourceSpan -> SourcePos
startPos (SourceSpan (start, _)) = start

showStart :: SourceSpan -> String
showStart = sourcePosPretty . startPos

showEnd :: SourceSpan -> String
showEnd = sourcePosPretty . startPos

endPos :: SourceSpan -> SourcePos
endPos (SourceSpan (_, end)) = end

instance Show SourceSpan where
  -- show (SourceSpan (start, end)) = sourcePosPretty start ++ "->" ++ sourcePosPretty end
  show (SourceSpan (start, end)) = "META" 

srcSpan :: SourcePos -> SourcePos -> SourceSpan
srcSpan start end = SourceSpan (start, end)

type instance FunDecl ParsedP = SourceSpan
type instance FunDeclT ParsedP = Maybe Type
type instance VarDecl ParsedP = SourceSpan
type instance VarDeclT ParsedP = Maybe Type

deriving instance Eq (Decl ParsedP)
deriving instance Show (Decl ParsedP)

type instance AssignStmt ParsedP = SourceSpan
type instance ReturnStmt ParsedP = SourceSpan
type instance IfStmt ParsedP = SourceSpan
type instance WhileStmt ParsedP = SourceSpan
type instance ExprStmt ParsedP = SourceSpan
type instance VarStmt ParsedP = SourceSpan

deriving instance Eq (Stmt ParsedP)
deriving instance Show (Stmt ParsedP)

type instance BinOpExpr ParsedP = SourceSpan
type instance UnaryOpExpr ParsedP = SourceSpan
type instance FunctionCallExpr ParsedP = SourceSpan
type instance VariableExpr ParsedP = SourceSpan
type instance LiteralExpr ParsedP = SourceSpan

deriving instance Eq (Expr ParsedP)
deriving instance Show (Expr ParsedP)

deriving instance Eq (Literal ParsedP)
deriving instance Show (Literal ParsedP)

