{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.TypeSpec (typeSpec) where

import SPL.Parser.AST (Type(IntType, CharType, BoolType, TupleType, TypeVar, ListType, VoidType))
import SPL.Parser.Type (pIntType, pCharType, pListType, pTypeVarType, pRetType, pType, pVoidType, pTupleType, pBoolType)

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)
import Text.Megaparsec (parse)

typeSpec :: Spec
typeSpec = do
    describe "Parser.Type" $ do
        describe "pRetType" $ do
            it "parses a return type" $ do
                parse pRetType "test.spl" "Int" `shouldParse` IntType
                parse pRetType "test.spl" "[a]" `shouldParse` ListType (TypeVar "a")

            it "parses the Void type" $ do
                parse pRetType "test.spl" "Void" `shouldParse` VoidType

        describe "pType" $ do
            it "parses a return type" $ do
                parse pType "test.spl" "Int" `shouldParse` IntType
                parse pType "test.spl" "[a]" `shouldParse` ListType (TypeVar "a")

            it "doesn't parse the Void type" $ do
                parse pType "test.spl" `shouldFailOn` "Void"

        describe "pIntType" $ do
            it "parses the Int type" $ do
                parse pIntType "test.spl" "Int" `shouldParse` IntType
            
        describe "pCharType" $ do
            it "parses the Char type" $ do
                parse pCharType "test.spl" "Char" `shouldParse` CharType

        describe "pBoolType" $ do
            it "parses the Bool type" $ do
                parse pBoolType "test.spl" "Bool" `shouldParse` BoolType

        describe "pTupleType" $ do
            it "parses the a simple tuple type" $ do
                parse pTupleType "test.spl" "(Int, Int)" `shouldParse` TupleType IntType IntType
                parse pTupleType "test.spl" "(Int, Char)" `shouldParse` TupleType IntType CharType

            it "parses a tuple with type variables" $ do
                parse pTupleType "test.spl" "(Int, a)" `shouldParse` TupleType IntType (TypeVar "a")
                parse pTupleType "test.spl" "(a, Int)" `shouldParse` TupleType (TypeVar "a") IntType
                parse pTupleType "test.spl" "(a, b)" `shouldParse` TupleType (TypeVar "a") (TypeVar "b")

            it "parses a nested tuple type" $ do
                parse pTupleType "test.spl" "(Int, (a, b))" `shouldParse` TupleType IntType (TupleType (TypeVar "a") (TypeVar "b"))

        describe "pListType" $ do
            it "parses a simple list type" $ do
                parse pListType "test.spl" "[Int]" `shouldParse` ListType IntType

            it "parses a polymorphic list type" $ do
                parse pListType "test.spl" "[a]" `shouldParse` ListType (TypeVar "a")

        describe "pTypeVarType" $ do
            it "parses a type var" $ do
                parse pTypeVarType "test.spl" "a" `shouldParse` TypeVar "a"

            it "doesn't parse basic types" $ do
                parse pTypeVarType "test.spl" `shouldFailOn` "Int"
                parse pTypeVarType "test.spl" `shouldFailOn` "Char"
                parse pTypeVarType "test.spl" `shouldFailOn` "Bool"
                parse pTypeVarType "test.spl" `shouldFailOn` "Void"
        
        describe "pVoidType" $ do
            it "parses the Void type" $ do
                parse pVoidType "test.spl" "Void" `shouldParse` VoidType