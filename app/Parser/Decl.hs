module Parser.Decl where

import Parser.AST
import Parser.Parser (Parser)
import Text.Megaparsec (choice, try, optional, many)
import Control.Monad (void)
import Parser.Expr (pExpr)
import qualified Parser.Lexer as L
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Parser.Type (pType, pRetType)
import Parser.Stmt (pStmt)
import Utils (optionList)

-- Parses any declaration.
pDecl :: Parser Decl
pDecl = choice
    [ try pVarDecl
    , try pFunDecl
    ]

-- Parses a variable declaration.
-- Grammar: ('var' | type) identifier '=' expr ';'
pVarDecl :: Parser Decl
pVarDecl = do
    ty <- pVarType
    identifier <- T.unpack <$> L.tIdentifier
    void L.tEq
    expr <- pExpr
    void L.tSemiColon

    return $ VarDecl ty identifier expr

    where pVarType = (L.tVar $> Nothing) <|> (Just <$> pType)

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

    return $ FunDecl functionName retType (optionList args) statements

    where 
        pArg = do
            name <- T.unpack <$> L.tIdentifier
            ty <- optional $ do
                void L.tColon
                pType
            return (name, ty)

        