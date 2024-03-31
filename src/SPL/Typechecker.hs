{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module SPL.Parser where

import SPL.Parser.AST
import SPL.Parser.Parser (SourceSpan)

type instance FunDecl TypecheckP = SourceSpan

type instance ReturnStmt TypecheckP = SourceSpan
type instance IfStmt TypecheckP = SourceSpan
type instance WhileStmt TypecheckP = SourceSpan
type instance ExprStmt TypecheckP = SourceSpan
type instance VarStmt TypecheckP = SourceSpan

type instance BinOpExpr TypecheckP = (SourceSpan, Type)
type instance UnaryOpExpr TypecheckP = (SourceSpan, Type)
type instance AssignExpr TypecheckP = (SourceSpan, Type)
type instance FunctionCallExpr TypecheckP = (SourceSpan, Type)
type instance VariableExpr TypecheckP = (SourceSpan, Type)
type instance LiteralExpr TypecheckP = (SourceSpan, Type)
