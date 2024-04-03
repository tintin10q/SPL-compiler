{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module SPL.Parser.Expr where

import SPL.Parser.AST
import SPL.Parser.Parser (srcSpan, Parser)
import qualified SPL.Parser.Lexer as L

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Text.Megaparsec (choice, try, (<|>), many, optional, getSourcePos)
import qualified Data.Text as T

{--

Expression parsers.

--}

-- Operator table ordered in decreasing precedence (i.e. the higher
-- in the list, the greater the binding strength of the set of
-- operators). Operators in the same inner list have the same precedence.
-- Associativity can be modified by using constructors from the
-- Operator datatype, which supports:
--
-- InfixN: non-associative infix
-- InfixL: left-associative infix
-- InfixR: right-associative infix
-- Prefix: prefix
-- Postfix: postfix
--
-- More info: https://markkarpov.com/tutorial/megaparsec.html#parsing-expressions
-- Operator precedence and associativity: https://rosettacode.org/wiki/Operator_precedence#Haskell
operatorTable :: [[Operator Parser (Expr ParsedP)]]
operatorTable = [[]]
    -- [ [ Postfix (unary (UnaryOpExpr (FieldAccess HeadField)) (try (L.tDot <* L.tHead)))
    --   , Postfix (unary (UnaryOpExpr (FieldAccess TailField)) (try (L.tDot <* L.tTail)))
    --   ]
    -- , [ Prefix (unary (UnaryOpExpr Negate) L.tExcl)
    --   ]
    -- , [ InfixL (binary (BinOpExpr Mul) L.tStar)
    --   , InfixL (binary (BinOpExpr Div) L.tSlash)
    --   , InfixL (binary (BinOpExpr Mod) L.tPercent)
    --   ]
    -- , [ InfixL (binary (BinOpExpr Add) L.tPlus)
    --   , InfixL (binary (BinOpExpr Sub) L.tMin)
    --   ]
    -- , [ InfixR (binary (BinOpExpr Cons) L.tColon)]
    -- , [ InfixN (binary (BinOpExpr Gt) L.tGt)
    --   , InfixN (binary (BinOpExpr Gte) L.tGte)
    --   , InfixN (binary (BinOpExpr Lt) L.tLt)
    --   , InfixN (binary (BinOpExpr Lte) L.tLte)
    --   , InfixN (binary (BinOpExpr Eq) L.tDoubleEq)
    --   , InfixN (binary (BinOpExpr Neq) L.tExclEq)
    --   ]
    -- , [ InfixR $ binary (BinOpExpr And) L.tDoubleAmpersand  ]
    -- , [ InfixR $ binary (BinOpExpr Or) L.tDoublePipe ]
    -- ]

-- Parses an expression. For operator precedence, see operatorTable.
pExpr :: Parser (Expr ParsedP)
pExpr = makeExprParser pTerm operatorTable

-- Parses a term expression (everything that is not a binary 
-- or unary 'opped' expression).
pTerm :: Parser (Expr ParsedP)
pTerm = choice
  [ try $ L.parens pExpr
  , try pFunctionCall
  , try pAssignExpr
  , try pLiteralExpr
  , try pVariableExpr
  ]

-- Parses a function call expression (e.g. foo(), bar('a', 'b', 'c')).
pFunctionCall :: Parser (Expr ParsedP)
pFunctionCall = do
  posStart <- getSourcePos
  functionName <- T.unpack <$> L.tIdentifier
  void L.tLeftParen
  args <- optional $ do
    first <- pExpr
    others <- many $ L.tComma *> pExpr
    return $ first:others
  void L.tRightParen
  posEnd <- getSourcePos
  return $ FunctionCallExpr (srcSpan posStart posEnd) functionName $ concat args

-- Parses an assignment expression (e.g. a = 'c', a.b = 'd').
pAssignExpr :: Parser (Expr ParsedP)
pAssignExpr = do
  posStart <- getSourcePos
  variable <- pVariable
  void L.tEq
  expr <- pExpr
  posEnd <- getSourcePos
  return $ AssignExpr (srcSpan posStart posEnd) variable expr

-- Parses a literal expression (e.g. 10, 'a', []).
pLiteralExpr :: Parser (Expr ParsedP)
pLiteralExpr =  do
  posStart <- getSourcePos
  lit <- pLiteral
  posEnd <- getSourcePos
  return $ LiteralExpr (srcSpan posStart posEnd) lit

-- Parses a variable expression (e.g. a, a.b, a.b.c).
pVariableExpr :: Parser (Expr ParsedP)
pVariableExpr = do
  posStart <- getSourcePos
  v <- pVariable
  posEnd <- getSourcePos
  return $ VariableExpr (srcSpan posStart posEnd) v

-- Parses a variable (e.g. a, a.b., a.b.c).
pVariable :: Parser Variable
pVariable = do
  identifier <- T.unpack <$> L.lexeme L.tIdentifier
  field <- optional pField
  return $ Identifier identifier field
  where pField = L.tDot *> (try (HeadField <$ L.tHead) <|> try (TailField <$ L.tTail))

-- Parse any literal value
pLiteral :: Parser (Literal ParsedP)
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
-- Grammar: 'true'
pTrue :: Parser (Literal ParsedP)
pTrue = do
  void L.tTrue
  return TrueLit

-- Parse false.
-- Grammar: 'false'
pFalse :: Parser (Literal ParsedP)
pFalse = do
  void L.tFalse
  return FalseLit

-- Parses a signed floating point number (e.g. 12.0, -12.0, +12.0).
pFloat :: Parser (Literal ParsedP)
pFloat = do
  f <- L.tFloat
  return $ FloatLit f

-- Parse a signed integer (e.g. 12, -12, +12).
-- Grammar (simplified): [('-' | '+')] digit+
pInt :: Parser (Literal ParsedP)
pInt = do
  i <- L.tInteger
  return $ IntLit i

-- Parses a character surrounded by quotes, including escape sequences
-- such as '\n' and '\t'.
-- Grammar: '\'' any char '\''
pChar :: Parser (Literal ParsedP)
pChar = do
  c <- L.tChar
  return $ CharLit c

-- Parses a tuple of exactly two expressions.
-- Grammar: '(' expr ',' expr ')'
pTuple :: Parser (Literal ParsedP)
pTuple = L.parens $ do
  left <- pExpr
  void L.tComma
  right <- pExpr
  return $ TupleLit (left, right)

-- Parses the empty list ([]).
-- Grammar: '[]'
pEmptyList :: Parser (Literal ParsedP)
pEmptyList = do
  void L.tEmptyList
  return EmptyListLit