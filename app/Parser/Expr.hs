{-# LANGUAGE OverloadedStrings #-}
module Parser.Expr where

import Parser.AST
import Parser.Parser (Parser)
import Text.Megaparsec (choice, try)
import Parser.Lexer
import Control.Monad (void)
import Control.Monad.Combinators.Expr

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

pTerm :: Parser Expr
pTerm = LiteralExpr <$> pLiteral

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [
        Prefix (UnaryOp Negate <$ symbol "!")
      ],
      [
        InfixL (BinOp Mul <$ symbol "*"),
        InfixL (BinOp Div <$ symbol "/")
      ],
      [
        InfixL (BinOp Add <$ symbol "+"),
        InfixL (BinOp Sub <$ symbol "-")
      ]
    ]

-- Literals

-- Parse any literal value
pLiteral :: Parser Literal
pLiteral = choice
    [ try pTrue
    , try pFalse
    , try pFloat
    , try pInt
    , try pChar
    , try pTuple
    , try pEmptyList
    ]

-- Parse true.
pTrue :: Parser Literal
pTrue = TrueLit <$ tTrue

-- Parse false.
pFalse :: Parser Literal
pFalse = FalseLit <$ tFalse

-- Parses a signed floating point number (12.0, -12.0, +12.0).
pFloat :: Parser Literal
pFloat = FloatLit <$> tFloat

-- Parse a signed integer (12, -12, +12).
pInt :: Parser Literal
pInt = IntLit <$> tInteger

-- Parses a character surrounded by quotes, including escape sequences
-- such as '\n' and '\t'.
pChar :: Parser Literal
pChar = CharLit <$> tChar

-- Parses a tuple of exactly two expressions.
pTuple :: Parser Literal
pTuple = parens $ do
    left <- pExpr
    void tComma
    right <- pExpr
    return $ TupleLit (left, right)

-- Parses the empty list ([]).
pEmptyList :: Parser Literal
pEmptyList = EmptyListLit <$ tEmptyList