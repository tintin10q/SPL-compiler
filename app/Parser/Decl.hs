module Parser.Decl where

import Parser.AST
import Parser.Parser (Parser)
import Text.Megaparsec (optional, many)
import Control.Monad (void)
import qualified Parser.Lexer as L
import qualified Data.Text as T
import Parser.Type (pType, pRetType)
import Parser.Stmt (pStmt)

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

        