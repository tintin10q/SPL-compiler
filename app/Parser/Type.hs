{-# LANGUAGE OverloadedStrings #-}
module Parser.Type where

import Parser.AST 
import Parser.Parser
import Text.Megaparsec (try, choice)
import qualified Parser.Lexer as L
import qualified Data.Text as T
import Control.Monad (void)

-- Parses a return type (which is a regular type, with 'void')
pRetType :: Parser Type 
pRetType = choice 
    [ try pType
    , try pVoidType
    ]

-- Parses any (regular) type.
pType :: Parser Type 
pType = choice 
    [ try pIntType
    , try pCharType
    , try pBoolType
    , try pTupleType
    , try pListType
    , try pTypeVarType
    ]

-- Parses the integer type.
-- Grammar: 'Int'
pIntType :: Parser Type 
pIntType = IntType <$ L.tIntType

-- Parses the char type.
-- Grammar: 'char'
pCharType :: Parser Type 
pCharType = CharType <$ L.tCharType

-- Parses the boolean type.
-- Grammar: 'Bool'
pBoolType :: Parser Type 
pBoolType = BoolType <$ L.tBoolType

-- Parses the tuple type.
-- Grammar: '(' type ',' type ')'
pTupleType :: Parser Type
pTupleType = do
    void L.tLeftParen
    tyLeft <- pType
    void L.tComma
    tyRight <- pType
    void L.tRightParen

    return $ TupleType tyLeft tyRight

-- Parses the list type.
-- Grammar: '[' type ']'
pListType :: Parser Type 
pListType = do
    void L.tLeftSquareBracket
    ty <- pType
    void L.tRightSquareBracket

    return $ ListType ty

-- Parses a type variable.
-- Grammar: identifier
pTypeVarType :: Parser Type
pTypeVarType = TypeVar . T.unpack <$> L.tIdentifier

-- Parses the void type.
-- Grammar: 'Void'
pVoidType :: Parser Type 
pVoidType = VoidType <$ L.tVoidType

