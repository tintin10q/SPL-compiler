{-# LANGUAGE DataKinds #-}
module SPL.Parser.Decl where

import SPL.AST
import SPL.Parser.Parser (Parser)
import SPL.Parser.SourceSpan
import SPL.Parser.Stmt (pStmt)
import SPL.Parser.Type (pType, pRetType)
import qualified SPL.Parser.Lexer as L

import Control.Monad (void)
import Text.Megaparsec (optional, many, getSourcePos, (<|>), try)
import qualified Data.Text as T
import SPL.Parser.Expr (pExpr)
import Data.Functor (($>))

-- Parses any declaration.
pDecl :: Parser (Decl ParsedP) 
pDecl =  try pVarDecl <|> pFunDecl
-- todo We could do a lookahead here to parse the type first? If we have that then we know its a vardecl

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
    where pVarType = (try L.tVar $> Nothing) <|> (Just <$> pType)

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
    varDecls <- many $ try pVarDecl
    -- varDecls <- many $ try pDecl -- todo, this allows nested function definitions but we are just doing globals 
    statements <- many pStmt
    void L.tRightBrace
    posEnd <- getSourcePos

    return $ FunDecl (srcSpan posStart posEnd) functionName retType (concat args) varDecls statements

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

-- Parses a variable declaration.
-- Grammar: ('var' | type) identifier '=' expr ';'
{- todo Remove this
pFunVarDecl :: Parser (FunVarDecl ParsedP)
pFunVarDecl = do
    posStart <- getSourcePos
    ty <- pVarType
    identifier <- T.unpack <$> L.tIdentifier
    void L.tEq
    expr <- pExpr
    void L.tSemiColon
    posEnd <- getSourcePos
    return $ FunVarDeclConstr (srcSpan posStart posEnd) identifier ty expr
    where pVarType = (L.tVar $> Nothing) <|> (Just <$> pType)
-} 