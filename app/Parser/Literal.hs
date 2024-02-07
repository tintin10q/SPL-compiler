{-# LANGUAGE OverloadedStrings #-}
module Parser.Literal where

import Parser.AST (Literal (..))
import Parser.Parser (Parser)
import Parser.Lexer
import Parser.Expr (pExpr)
import Control.Monad (void)
import Text.Megaparsec (choice, try)

pLiteral :: Parser Literal
pLiteral = choice
    [ try pTrue
    , try pFalse
    , try pInt
    , try pFloat
    , try pChar
    , try pTuple
    , try pEmptyList
    ]

pTrue :: Parser Literal
pTrue = TrueLit <$ tTrue

pFalse :: Parser Literal
pFalse = FalseLit <$ tFalse

pInt :: Parser Literal
pInt = IntLit <$> tInteger

pFloat :: Parser Literal
pFloat = FloatLit <$> tFloat

pChar :: Parser Literal
pChar = CharLit <$> tChar

pTuple :: Parser Literal
pTuple = parens $ do
    left <- pExpr
    void tComma
    right <- pExpr
    return $ TupleLit (left, right)

pEmptyList :: Parser Literal
pEmptyList = EmptyListLit <$ tEmptyList