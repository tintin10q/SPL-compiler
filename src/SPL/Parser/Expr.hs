module SPL.Parser.Expr where

import SPL.Parser.AST
import SPL.Parser.Parser (Parser)
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
operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ Postfix (unary (UnaryOpExpr (FieldAccess HeadField)) (try (L.tDot <* L.tHead)))
      , Postfix (unary (UnaryOpExpr (FieldAccess TailField)) (try (L.tDot <* L.tTail)))
      ]
    , [ Prefix (unary (UnaryOpExpr Negate) L.tExcl)
      ]
    , [ InfixL (binary (BinOpExpr Mul) L.tStar)
      , InfixL (binary (BinOpExpr Div) L.tSlash)
      , InfixL (binary (BinOpExpr Mod) L.tPercent)
      ]
    , [ InfixL (binary (BinOpExpr Add) L.tPlus)
      , InfixL (binary (BinOpExpr Sub) L.tMin)
      ]
    , [ InfixR (binary (BinOpExpr Cons) L.tColon)]
    , [ InfixN (binary (BinOpExpr Gt) L.tGt)
      , InfixN (binary (BinOpExpr Gte) L.tGte)
      , InfixN (binary (BinOpExpr Lt) L.tLt)
      , InfixN (binary (BinOpExpr Lte) L.tLte)
      , InfixN (binary (BinOpExpr Eq) L.tDoubleEq)
      , InfixN (binary (BinOpExpr Neq) L.tExclEq)
      ]
    , [ InfixR $ binary (BinOpExpr And) L.tDoubleAmpersand  ]
    , [ InfixR $ binary (BinOpExpr Or) L.tDoublePipe ]
    ] where
        unary :: (Expr -> ExprU) -> Parser a -> Parser (Expr -> Expr)
        unary f p = do
          startPos <- getSourcePos
          void p
          endPos <- getSourcePos
          return $ \e -> Expr (mkAnnotation startPos endPos, f e)

        binary :: (Expr -> Expr -> ExprU) -> Parser a -> Parser (Expr -> Expr -> Expr)
        binary f p = do
          startPos <- getSourcePos
          void p
          endPos <- getSourcePos
          return $ \l r -> Expr (mkAnnotation startPos endPos, f l r)


-- Parses an expression. For operator precedence, see operatorTable.
pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

-- Parses a term expression (everything that is not a binary 
-- or unary 'opped' expression).
pTerm :: Parser Expr
pTerm = choice
  [ try $ L.parens pExpr
  , try $ annotated pFunctionCall
  , try $ annotated pAssignExpr
  , try $ annotated pLiteralExpr
  , try $ annotated pVariableExpr
  ]

-- Parses a function call expression (e.g. foo(), bar('a', 'b', 'c')).
pFunctionCall :: Parser ExprU
pFunctionCall = do
  functionName <- T.unpack <$> L.tIdentifier
  void L.tLeftParen
  args <- optional $ do
    first <- pExpr
    others <- many $ L.tComma *> pExpr
    return $ first:others
  void L.tRightParen
  return $ FunctionCallExpr functionName $ concat args

-- Parses an assignment expression (e.g. a = 'c', a.b = 'd').
pAssignExpr :: Parser ExprU
pAssignExpr = do
  variable <- annotated pVariable
  void L.tEq
  AssignExpr variable <$> pExpr

-- Parses a literal expression (e.g. 10, 'a', []).
pLiteralExpr :: Parser ExprU
pLiteralExpr = LiteralExpr <$> annotated pLiteral

-- Parses a variable expression (e.g. a, a.b, a.b.c).
pVariableExpr :: Parser ExprU
pVariableExpr = VariableExpr <$> annotated pVariable

-- Parses a variable (e.g. a, a.b., a.b.c).
pVariable :: Parser VariableU
pVariable = do
  identifier <- T.unpack <$> L.lexeme L.tIdentifier
  field <- optional pField
  return $ Identifier identifier field
  where pField = L.tDot *> (try (HeadField <$ L.tHead) <|> try (TailField <$ L.tTail))

-- Parse any literal value
pLiteral :: Parser LiteralU
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
pTrue :: Parser LiteralU
pTrue = TrueLit <$ L.tTrue

-- Parse false.
-- Grammar: 'false'
pFalse :: Parser LiteralU
pFalse = FalseLit <$ L.tFalse

-- Parses a signed floating point number (e.g. 12.0, -12.0, +12.0).
pFloat :: Parser LiteralU
pFloat = FloatLit <$> L.tFloat

-- Parse a signed integer (e.g. 12, -12, +12).
-- Grammar (simplified): [('-' | '+')] digit+
pInt :: Parser LiteralU
pInt = IntLit <$> L.tInteger

-- Parses a character surrounded by quotes, including escape sequences
-- such as '\n' and '\t'.
-- Grammar: '\'' any char '\''
pChar :: Parser LiteralU
pChar = CharLit <$> L.tChar

-- Parses a tuple of exactly two expressions.
-- Grammar: '(' expr ',' expr ')'
pTuple :: Parser LiteralU
pTuple = L.parens $ do
  left <- pExpr
  void L.tComma
  right <- pExpr
  return $ TupleLit (left, right)

-- Parses the empty list ([]).
-- Grammar: '[]'
pEmptyList :: Parser LiteralU
pEmptyList = EmptyListLit <$ L.tEmptyList