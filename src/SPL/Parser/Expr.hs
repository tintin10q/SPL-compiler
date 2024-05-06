{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module SPL.Parser.Expr where

import SPL.Parser.AST
import SPL.Parser.Parser (srcSpan, Parser, SourceSpan, startPos, endPos)
import qualified SPL.Parser.Lexer as L

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Text.Megaparsec (choice, try, (<|>), many, optional, getSourcePos)
import qualified Data.Text as T
import Data.Functor (($>))
import Debug.Trace (trace)

{--

Expression parsers.

--}

exprSpan :: Expr ParsedP -> SourceSpan
exprSpan expr = case expr of
  BinOpExpr srcSpan _ _ _ -> srcSpan
  UnaryOpExpr srcSpan _ _ -> srcSpan
  FunctionCallExpr srcSpan _ _ -> srcSpan
  VariableExpr srcSpan _ -> srcSpan
  LiteralExpr srcSpan _ -> srcSpan

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
operatorTable =
    [ [ Postfix (unary (FieldAccess HeadField) (try (L.tDot <* L.tHead)))
      , Postfix (unary (FieldAccess TailField) (try (L.tDot <* L.tTail)))
      ]
    , [ Prefix (unary Negate L.tExcl)
      ]
    , [ InfixL (binary Mul L.tStar)
      , InfixL (binary Div L.tSlash)
      , InfixL (binary Mod L.tPercent)
      ]
    , [ InfixL (binary Add L.tPlus)
      , InfixL (binary Sub L.tMin)
      ]
    , [ InfixR (binary Cons L.tColon)]
    , [ InfixN (binary Gt L.tGt)
      , InfixN (binary Gte L.tGte)
      , InfixN (binary Lt L.tLt)
      , InfixN (binary Lte L.tLte)
      , InfixN (binary Eq L.tDoubleEq)
      , InfixN (binary Neq L.tExclEq)
      ]
    , [ InfixR $ binary And L.tDoubleAmpersand  ]
    , [ InfixR $ binary Or L.tDoublePipe ]
    ]
    where binary def pSymbol = pSymbol $> \e1 e2 ->
            BinOpExpr (srcSpan (startPos $ exprSpan e1) (endPos $ exprSpan e2)) def e1 e2
          unary def pSymbol = pSymbol $> \e ->
            UnaryOpExpr (exprSpan e) def e

-- Parses an expression. For operator precedence, see operatorTable.
pExpr :: Parser (Expr ParsedP)
pExpr = makeExprParser pTerm operatorTable

-- Parses a term expression (everything that is not a binary 
-- or unary 'opped' expression).
pTerm :: Parser (Expr ParsedP)
pTerm = choice
  [ try $ L.parens pExpr
  , try pFunctionCall
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
  identifier <- T.unpack <$> L.tIdentifier
  -- field <- optional pField
  return $ Identifier identifier Nothing
  -- where pField = L.tDot *> (try (HeadField <$ L.tHead) <|> try (TailField <$ L.tTail))

-- Parse any literal value
pLiteral :: Parser (Literal ParsedP)
pLiteral = choice
    [ try pTrue
    , try pFalse
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

-- Parse a signed integer (e.g. 12, -12, +12).
-- Grammar (simplified): [('-' | '+')] digit+
pInt :: Parser (Literal ParsedP)
pInt = do
  IntLit <$> L.tInteger

-- Parses a character surrounded by quotes, including escape sequences
-- such as '\n' and '\t'.
-- Grammar: '\'' any char '\''
pChar :: Parser (Literal ParsedP)
pChar = do
  CharLit <$> L.tChar

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