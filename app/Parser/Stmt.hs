module Parser.Stmt where

import Parser.AST
import Parser.Parser (Parser)
import Parser.Expr (pExpr)
import Text.Megaparsec (choice, try, optional, many)
import Control.Monad (void)
import qualified Parser.Lexer as L

-- Parses any statement.
pStmt :: Parser Stmt
pStmt = choice
    [ try pIfStmt
    , try pWhileStmt
    , try pReturnStmt
    , try pExprStmt
    ]

-- Parses an if statement.
-- Grammar: 'if' '(' expr ')' '{' stmt* '}' ['else' '{' stmt* '}']
pIfStmt :: Parser Stmt
pIfStmt = do
    void L.tIf                    -- 'if'
    void L.tLeftParen             -- '('
    condition <- pExpr            -- expr
    void L.tRightParen            -- ')'
    void L.tLeftBrace             -- '{'
    consequent <- many pStmt      -- stmt*
    void L.tRightBrace            -- '}'
    alternative <- optional $ do  -- [
        void L.tElse              -- 'else'
        void L.tLeftBrace         -- '{'
        alternative <- many pStmt -- stmt*
        void L.tRightBrace        -- '}'
        return alternative        -- ]

    return $ IfStmt condition consequent alternative

-- Parses a while statement.
-- Grammar: 'while' '(' expr ')' '{' stmt* '}'
pWhileStmt :: Parser Stmt
pWhileStmt = do
    void L.tWhile           -- 'while'
    void L.tLeftParen       -- '('
    condition <- pExpr      -- expr
    void L.tRightParen      -- ')'
    void L.tLeftBrace       -- '{'
    statement <- many pStmt -- stmt*
    void L.tRightBrace      -- '}'

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

