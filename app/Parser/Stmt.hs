{-# LANGUAGE OverloadedStrings #-}
module Parser.Stmt where

import Parser.AST
import Parser.Parser (Parser)
import Parser.Expr (pExpr, pVariable)
import qualified Parser.Lexer as L

import Text.Megaparsec (choice, try, (<|>), optional, many)
import Parser.Type (pVarType)


pReturn :: Parser Stmt
pReturn = ReturnStmt <$> (L.tReturn *> pExpr) <* L.tSemiColon

pVarStmt :: Parser Stmt
pVarStmt = VarStmt <$> (optional pVarType <|> Nothing <$ L.tVar) <*> pVariable <*> (L.tEq *> pExpr) <* L.tSemiColon

pIfStmt :: Parser Stmt
pIfStmt = IfStmt <$> (L.tIf *> L.parens pExpr) <*> pStmt <*> optional (L.tElse *> pStmt)

pWhileStmt :: Parser Stmt
pWhileStmt = WhileStmt <$> (L.tWhile *> L.parens pExpr) <*> pStmt


pForStmt :: Parser Stmt
pForStmt = do
    _ <- L.tFor
    _ <- L.symbol "("
    decl <- pVarStmt
    cond <- pExpr
    _ <- L.tSemiColon
    expr <- pExpr
    _ <- L.symbol ")"
    body <- pStmt
    return $ BlockStmt [
                decl, 
                WhileStmt cond $ ExprStmt expr <> body 
             ]

pExprStmt :: Parser Stmt
pExprStmt = ExprStmt <$> pExpr <* L.tSemiColon

pBlockStmt :: Parser Stmt
pBlockStmt = BlockStmt <$> (L.braces pStmt *> many pStmt)


pStmt :: Parser Stmt
pStmt = choice [ try pReturn, try pExprStmt, try pVarStmt, try pIfStmt, try pBlockStmt]

