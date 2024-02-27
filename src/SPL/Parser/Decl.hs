module SPL.Parser.Decl where

import SPL.Parser.AST
import SPL.Parser.Parser (Parser)
import SPL.Parser.Stmt (pStmt)
import SPL.Parser.Type (pType, pRetType)
import qualified SPL.Parser.Lexer as L

import Control.Monad (void)
import Text.Megaparsec (optional, many)
import qualified Data.Text as T

-- Parses any declaration.
pDecl :: Parser Decl
pDecl = pFunDecl

-- Parses a function declaration.
-- Grammar: identifier '(' [ funArgs ] ')' [ ':' retType ] '{' varDecl* stmt+ '}'
pFunDecl :: Parser Decl
pFunDecl = do
    functionName <- T.unpack <$> L.tIdentifier
    void L.tLeftParen
    args <- optional $ do
        first <- pArg
        rest <- many $ L.tComma *> pArg
        return $ first:rest
    void L.tRightParen
    retType <- optional $ do
        void L.tColon
        pRetType
    void L.tLeftBrace
    statements <- many pStmt
    void L.tRightBrace

    return $ FunDecl functionName retType (concat args) statements

    where 
        pArg = do
            name <- T.unpack <$> L.tIdentifier
            ty <- optional $ do
                void L.tColon
                pType
            return (name, ty)

        