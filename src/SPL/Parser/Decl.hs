{-# LANGUAGE DataKinds #-}
module SPL.Parser.Decl where

import SPL.Parser.AST
import SPL.Parser.Parser (Parser, srcSpan)
import SPL.Parser.Stmt (pStmt)
import SPL.Parser.Type (pType, pRetType)
import qualified SPL.Parser.Lexer as L

import Control.Monad (void)
import Text.Megaparsec (optional, many, getSourcePos)
import qualified Data.Text as T

-- Parses any declaration.
pDecl :: Parser (Decl ParserP)
pDecl = pFunDecl

-- Parses a function declaration.
pFunDecl :: Parser (Decl ParserP)
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

        