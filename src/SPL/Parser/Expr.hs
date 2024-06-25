{-# LANGUAGE DataKinds #-}
module SPL.Parser.Expr where

import SPL.AST
import SPL.Parser.Parser (Parser)
import qualified SPL.Parser.Lexer as L

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Text.Megaparsec (choice, try, (<|>), many, optional, getSourcePos, noneOf, oneOf, sourcePosPretty)
import qualified Data.Text as T
import Data.Functor (($>))
import Text.Megaparsec.Char (char)
import SPL.Colors (red, green, black, yellow)
import SPL.Parser.SourceSpan ( endPos, showStart, srcSpan, startPos, SourceSpan )


{--

Expression parsers.

--}

{- Get meta info from expr -}
exprSpan :: Expr ParsedP -> SourceSpan
exprSpan expr = case expr of
  BinOpExpr meta _ _ _ -> meta
  UnaryOpExpr meta _ _ -> meta
  FunctionCallExpr meta _ _ -> meta
  VariableExpr meta _ -> meta
  LiteralExpr meta _ -> meta

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
      , Postfix (unary (FieldAccess SecondField) (try (L.tDot <* L.tSnd)))
      , Postfix (unary (FieldAccess FirstField) (try (L.tDot <* L.tFst)))
      ]
    , [ Prefix (unary Negate L.tExcl)
      ]
    , [ InfixL (binary Mul L.tStar)
      , InfixL (binary Div L.tSlash)
      , InfixL (binary Mod L.tPercent)
      ]
    , [ InfixL (binary Add L.tPlus)
      , InfixL (binary Sub L.tMin)
      , Prefix (unary Min L.tMin)
      ]
    , [ InfixR (binary Cons L.tColon)]
    , [
        InfixN (binary Gte L.tGte) -- Gte and Lte have to go before > and < because otherwise it thinks its = after
      , InfixN (binary Gt L.tGt)
      , InfixN (binary Lte L.tLte)
      , InfixN (binary Lt L.tLt)
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
  , try pStringExpr
  ]

stringUntilQuote :: Parser T.Text
stringUntilQuote = T.pack <$> many (noneOf ['"', '\n', '\r'])

escapedChar :: Parser Char
escapedChar = do
    _ <- char '\\'
    c <- oneOf ['\\', 'n', '"']
    return $ case c of
        'n' -> '\n'
        '\\' -> '\\'
        '"' -> '"'
        _ -> c  -- Shouldn't reach here

-- Parser for any string until a double quote, handling escape sequences
stringWithEscapes :: Parser T.Text
stringWithEscapes = T.pack <$> many (escapedChar <|> noneOf ['"', '\n', '\r', '\\'])


pStringExpr :: Parser (Expr ParsedP)
pStringExpr =  do
    posStart <- getSourcePos
    _ <- char '\"' -- We actually have to care about what is after this and not remove whitespace
    tokens <- reverse . T.unpack <$> stringWithEscapes
    _ <- L.tQuotation
    posEnd <- getSourcePos
    let meta = srcSpan posStart posEnd

    -- todo make this a nicer parse error from parsec
    if null tokens then error $ red "Empty string not allowed at " ++ showStart meta ++ ".\nThe string syntax is just syntactic sugar for a large cons expression.\nBut that means that \"\" == [] which is not ideal because the typeof "++ black "\"\"" ++" is "++ yellow "[Char]"++ " while typeof "++ black "[]" ++ " is "++yellow "[a]"++".\nBut the type checker can't know this anymore as this information is thrown away with the desugaring. So to prevent this confusion just no empty strings."
    else return $ foldl (\previous token -> BinOpExpr meta Cons (LiteralExpr meta $ CharLit token ) previous) (LiteralExpr meta EmptyListLit) tokens
  -- pStringExpr
  -- UnaryOpExpr _ op expr

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
  field <- optional pField
  return $ Identifier identifier field
  where pField = L.tDot *> (try (HeadField <$ L.tHead) <|> try (TailField <$ L.tTail) <|> try (FirstField <$ L.tFst) <|> try (SecondField <$ L.tSnd))

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


intUpperBound :: Int
intUpperBound = maxBound

intLowerBound :: Int
intLowerBound = minBound

checkNumberRange :: Integer -> Either String Int
checkNumberRange integer = let lowerbound = toInteger intLowerBound
                               upperbound = toInteger intUpperBound
                           in if integer > upperbound then Left $ red "Integer is too large for the compiler! " ++ black (show integer) ++ " > " ++ black (show upperbound) ++ ". "
                               else if integer < lowerbound then Left $ red "Integer is too small for the compiler! " ++ black (show integer) ++ " < " ++ black (show upperbound) ++ ". "
                               else Right $ fromInteger integer

-- Parse a signed integer (e.g. 12, -12, +12).
-- Grammar (simplified): [('-' | '+')] digit+
pInt :: Parser (Literal ParsedP)
pInt = do
  pos <- getSourcePos
  integer <- L.tInteger
  case checkNumberRange integer of
    Right int -> pure $ IntLit int
    Left msg -> error (msg ++ "At " ++ green (sourcePosPretty pos))

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