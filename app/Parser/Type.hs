{-# LANGUAGE OverloadedStrings #-}
module Parser.Type where

import Parser.AST 
import Parser.Parser
import Text.Megaparsec (try, choice)
import qualified Parser.Lexer as L
import qualified Data.Text as T
import Parser.Lexer (braces, brackets)

pIntType :: Parser Type 
pIntType = IntType <$ L.tIntType

pCharType :: Parser Type 
pCharType = CharType <$ L.tCharType

pBoolType :: Parser Type 
pBoolType = BoolType <$ L.tBoolType

pVoidType :: Parser Type 
pVoidType = VoidType <$ L.tVoidType

pTypeVarType :: Parser Type
pTypeVarType = TypeVar . T.unpack <$> L.tIdentifier

pTupleType :: Parser Type
pTupleType = TupleType <$> braces  pVarType <* L.tComma *> pVarType 

pListType :: Parser Type 
pListType = ListType <$> brackets pVarType

pFunctionType :: Parser Type 
pFunctionType = choice [try pIntType
                        , try pCharType
                        , try pBoolType
                        , try pVoidType
                        , pTypeVarType
                        ]

-- This is here because only functions can not have Void type. Right?
pVarType :: Parser Type 
pVarType = choice [try pIntType
                , try pCharType
                , try pBoolType
                , pTypeVarType
                ]
