{-# LANGUAGE OverloadedStrings #-}
module Parser.Expr where

import Parser.AST
import Parser.Parser (Parser)
import Text.Megaparsec (choice, try, (<|>), many, optional)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import qualified Parser.Lexer as L
import qualified Data.Text as T
import Utils (optionList)

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
operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ Prefix (UnaryOp Negate <$ L.tExcl)
      ]
    , [ InfixL (BinOp Mul <$ L.tStar)
      , InfixL (BinOp Div <$ L.tSlash)
      , InfixL (BinOp Mod <$ L.tPercent)
      ]
    , [ InfixL (BinOp Add <$ L.tPlus)
      , InfixL (BinOp Sub <$ L.tMin)
      ]
    , [ InfixR (BinOp Cons <$ L.tColon)]
    , [ InfixN (BinOp Gt <$ L.tGt)
      , InfixN (BinOp Gte <$ L.tGte)
      , InfixN (BinOp Lt <$ L.tLt)
      , InfixN (BinOp Lte <$ L.tLte)
      , InfixN (BinOp Eq <$ L.tDoubleEq)
      , InfixN (BinOp Neq <$ L.tExclEq)
      ]
    , [ InfixR (BinOp And <$ L.tDoubleAmpersand) ]
    , [ InfixR (BinOp Or <$ L.tDoublePipe) ]
    ]

-- Parses an expression. For operator precedence, see operatorTable.
pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

-- Parses a term expression (everything that is not a binary 
-- or unary 'opped' expression).
pTerm :: Parser Expr
pTerm = choice
  [ try $ L.parens pExpr
  , try pFunctionCall
  , try pAssignExpr
  , try pLiteralExpr
  , try pVariableExpr
  ]

-- Parses an assignment expression (e.g. a = 'c', a.b = 'd').
pAssignExpr :: Parser Expr
pAssignExpr = do
  variable <- pVariable
  void L.tEq
  AssignExpr variable <$> pExpr

-- Parses a function call expression (e.g. foo(), bar('a', 'b', 'c')).
pFunctionCall :: Parser Expr
pFunctionCall = do
  functionName <- T.unpack <$> L.tIdentifier
  void L.tLeftParen
  args <- optional $ do
    first <- pExpr
    others <- many $ L.tComma *> pExpr
    return $ first:others
  void L.tRightParen
  return $ FunctionCall functionName $ optionList args

-- Parses a variable expression (e.g. a, a.b, a.b.c).
pVariableExpr :: Parser Expr
pVariableExpr = VariableExpr <$> pVariable

-- Parses a literal expression (e.g. 10, 'a', []).
pLiteralExpr :: Parser Expr
pLiteralExpr = LiteralExpr <$> pLiteral

-- Parses a variable (e.g. a, a.b., a.b.c).
pVariable :: Parser Variable
pVariable = pIdentifier <|> pProperty

-- Parses an identifier (e.g. a).
-- Grammar: (alphaLower | '_') (alphaNum | '_')* '\''*
pIdentifier :: Parser Variable
pIdentifier = Identifier . T.unpack <$> L.lexeme L.tIdentifier

-- Parses a property (e.g. a.b, a.b.c).
pProperty :: Parser Variable
pProperty = fail "Not implementedd" -- TODO

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
-- Grammar: 'true'
pTrue :: Parser Literal
pTrue = TrueLit <$ L.tTrue

-- Parse false.
-- Grammar: 'false'
pFalse :: Parser Literal
pFalse = FalseLit <$ L.tFalse

-- Parses a signed floating point number (e.g. 12.0, -12.0, +12.0).
pFloat :: Parser Literal
pFloat = FloatLit <$> L.tFloat

-- Parse a signed integer (e.g. 12, -12, +12).
-- Grammar (simplified): [('-' | '+')] digit+
pInt :: Parser Literal
pInt = IntLit <$> L.tInteger

-- Parses a character surrounded by quotes, including escape sequences
-- such as '\n' and '\t'.
-- Grammar: '\'' any char '\''
pChar :: Parser Literal
pChar = CharLit <$> L.tChar

-- Parses a tuple of exactly two expressions.
-- Grammar: '(' expr ',' expr ')'
pTuple :: Parser Literal
pTuple = L.parens $ do
    left <- pExpr
    void L.tComma
    right <- pExpr
    return $ TupleLit (left, right)

-- Parses the empty list ([]).
-- Grammar: '[]'
pEmptyList :: Parser Literal
pEmptyList = EmptyListLit <$ L.tEmptyList