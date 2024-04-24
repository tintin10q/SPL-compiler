{-# LANGUAGE DataKinds #-}
module SPL.Parser.Decl where

import SPL.Parser.AST
import SPL.Parser.Parser (Parser, srcSpan)
import SPL.Parser.Stmt (pStmt)
import SPL.Parser.Type (pType, pRetType)
import qualified SPL.Parser.Lexer as L

import Control.Monad (void)
import Text.Megaparsec (optional, many, getSourcePos, (<|>))
import qualified Data.Text as T
import SPL.Parser.Expr (pExpr)
import Data.Functor (($>))

-- Parses any declaration.
pDecl :: Parser (Decl ParsedP)
pDecl = pFunDecl <|> pVarDecl

pVarDecl :: Parser (Decl ParsedP)
pVarDecl = do
    posStart <- getSourcePos
    ty <- pVarType
    identifier <- T.unpack <$> L.tIdentifier
    void L.tEq
    expr <- pExpr
    void L.tSemiColon
    posEnd <- getSourcePos
    return $ VarDecl (srcSpan posStart posEnd) identifier ty expr
    where pVarType = (L.tVar $> Nothing) <|> (Just <$> pType)

-- Parses a function declaration.
pFunDecl :: Parser (Decl ParsedP)
pFunDecl = do
    posStart <- getSourcePos
    functionName <- T.unpack <$> L.tIdentifier
    void L.tLeftParen
    args <- optional pArgs
    void L.tRightParen
    retType <- optional $ do
        void L.tColon
        pRetType
    void L.tLeftBrace
    statements <- many pStmt
    void L.tRightBrace
    posEnd <- getSourcePos

    return $ FunDecl (srcSpan posStart posEnd) functionName retType (concat args) statements

    where
        pArgs = do
            first <- pArg
            rest <- many $ L.tComma *> pArg
            return $ first:rest
        pArg = do
            name <- T.unpack <$> L.tIdentifier
            ty <- optional $ do
                void L.tColon
                pType
            return (name, ty)

        