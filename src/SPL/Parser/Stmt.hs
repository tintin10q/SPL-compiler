module SPL.Parser.Stmt where

import SPL.Parser.AST
import SPL.Parser.Expr (pExpr)
import SPL.Parser.Parser (Parser)
import SPL.Parser.Type (pType)
import qualified SPL.Parser.Lexer as L

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Functor (($>))
import Text.Megaparsec (choice, try, optional, many)
import qualified Data.Text as T


-- Parses any statement.
pStmt :: Parser Stmt
pStmt = choice
    [ try pIfStmt
    , try pWhileStmt
    , try pReturnStmt
    , try pExprStmt
    , try pVarStmt
    ]

-- Parses an if statement.
-- Grammar: 'if' '(' expr ')' '{' stmt* '}' ['else' '{' stmt* '}']
pIfStmt :: Parser Stmt
pIfStmt = do
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
    return $ IfStmt condition consequent alternative

-- Parses a while statement.
-- Grammar: 'while' '(' expr ')' '{' stmt* '}'
pWhileStmt :: Parser Stmt
pWhileStmt = do
    void L.tWhile <* void L.tLeftParen      -- 'while' '('
    condition <- pExpr                      -- expr
    void L.tRightParen <* void L.tLeftBrace -- ')' '{'
    statement <- many pStmt                 -- stmt*
    void L.tRightBrace                      -- '}'
    return $ WhileStmt condition statement

-- Parses an expression statement.
-- Grammar: expr ';'
pExprStmt :: Parser Stmt
pExprStmt = ExprStmt <$> pExpr <* L.tSemiColon

-- Parses a return statement.
-- Grammar: 'return' [ expr ] ';'
pReturnStmt :: Parser Stmt
pReturnStmt = do
    void L.tReturn         -- 'return'
    expr <- optional pExpr -- [ expr ]
    void L.tSemiColon      -- ';'
    return $ ReturnStmt expr

-- Parses a variable declaration.
-- Grammar: ('var' | type) identifier '=' expr ';'
pVarStmt :: Parser Stmt
pVarStmt = do
    ty <- pVarType
    identifier <- T.unpack <$> L.tIdentifier
    void L.tEq
    expr <- pExpr
    void L.tSemiColon
    return $ VarStmt ty identifier expr
    where pVarType = (L.tVar $> Nothing) <|> (Just <$> pType)
