{-# LANGUAGE DataKinds #-}
module SPL.Parser.Stmt where

import SPL.Parser.AST
import SPL.Parser.Expr (pExpr, pVariable)
import SPL.Parser.Parser (Parser, srcSpan)
import SPL.Parser.Type (pType)
import qualified SPL.Parser.Lexer as L

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Functor (($>))
import Text.Megaparsec (choice, try, optional, many, getSourcePos)
import qualified Data.Text as T


-- Parses any statement.
pStmt :: Parser (Stmt ParsedP)
pStmt = choice
    [ try pAssignStmt
    , try pIfStmt
    , try pWhileStmt
    , try pReturnStmt
    , try pExprStmt
    ]

pAssignStmt :: Parser (Stmt ParsedP)
pAssignStmt = do
  posStart <- getSourcePos
  variable <- pVariable
  void L.tEq
  expr <- pExpr
  void L.tSemiColon
  posEnd <- getSourcePos
  return $ AssignStmt (srcSpan posStart posEnd) variable expr

-- Parses an if statement.
-- Grammar: 'if' '(' expr ')' '{' stmt* '}' ['else' '{' stmt* '}']
pIfStmt :: Parser (Stmt ParsedP)
pIfStmt = do
    posStart <- getSourcePos
    void L.tIf <* void L.tLeftParen         -- 'if' '('
    condition <- pExpr                      -- expr
    void L.tRightParen <* void L.tLeftBrace -- ')' '{'
    consequent <- many pStmt                -- stmt*
    void L.tRightBrace                      -- '}'
    alternative <- optional $ do
        void L.tElse <* void L.tLeftBrace -- 'else' '{'
        alternative <- many pStmt         -- stmt*
        void L.tRightBrace                -- '}'
        return alternative
    posEnd <- getSourcePos
    return $ IfStmt (srcSpan posStart posEnd) condition consequent alternative

-- Parses a while statement.
-- Grammar: 'while' '(' expr ')' '{' stmt* '}'
pWhileStmt :: Parser (Stmt ParsedP)
pWhileStmt = do
    posStart <- getSourcePos
    void L.tWhile <* void L.tLeftParen      -- 'while' '('
    condition <- pExpr                      -- expr
    void L.tRightParen <* void L.tLeftBrace -- ')' '{'
    statement <- many pStmt                 -- stmt*
    void L.tRightBrace                      -- '}'
    posEnd <- getSourcePos
    return $ WhileStmt (srcSpan posStart posEnd) condition statement

-- Parses an expression statement.
-- Grammar: expr ';'
pExprStmt :: Parser (Stmt ParsedP)
pExprStmt = do
  posStart <- getSourcePos
  expr <- pExpr
  posEnd <- getSourcePos
  void L.tSemiColon
  return $ ExprStmt (srcSpan posStart posEnd) expr

-- Parses a return statement.
-- Grammar: 'return' [ expr ] ';'
pReturnStmt :: Parser (Stmt ParsedP)
pReturnStmt = do
    posStart <- getSourcePos
    void L.tReturn         -- 'return'
    expr <- optional pExpr -- [ expr ]
    void L.tSemiColon      -- ';'
    posEnd <- getSourcePos
    return $ ReturnStmt (srcSpan posStart posEnd) expr
