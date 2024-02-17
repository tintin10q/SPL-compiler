{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Decl where

import Test.Hspec
import Text.Megaparsec (parse)
import Parser.AST
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)
import Parser.Decl (pFunDecl, pDecl)

declSpec :: Spec
declSpec = do
    describe "Parser.Decl" $ do
        describe "pDecl" $ do
            it "parses a function declaration" $ do
                parse pDecl "test.spl" "foo(): Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [] [ExprStmt $ FunctionCall "foo" []]
                parse pDecl "test.spl" "foo(a, b : [a]): Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [("a", Nothing), ("b", Just $ ListType $ TypeVar "a")] [ExprStmt $ FunctionCall "foo" []]

        describe "pFunDecl" $ do
            it "parses a function declaration without arguments or return type" $ do
                parse pFunDecl "test.spl" "foo() { foo(); }" `shouldParse` FunDecl "foo" Nothing [] [ExprStmt $ FunctionCall "foo" []]
                parse pFunDecl "test.spl" "a() { foo(); }" `shouldParse` FunDecl "a" Nothing [] [ExprStmt $ FunctionCall "foo" []]

            it "parses a function declaration with return type, without arguments" $ do
                parse pFunDecl "test.spl" "foo(): Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [] [ExprStmt $ FunctionCall "foo" []]
                parse pFunDecl "test.spl" "foo() : Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [] [ExprStmt $ FunctionCall "foo" []]
                parse pFunDecl "test.spl" "foo() :Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [] [ExprStmt $ FunctionCall "foo" []]
                parse pFunDecl "test.spl" "foo():Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [] [ExprStmt $ FunctionCall "foo" []]
                parse pFunDecl "test.spl" "foo(): Int { foo(); }" `shouldParse` FunDecl "foo" (Just IntType) [] [ExprStmt $ FunctionCall "foo" []]
                parse pFunDecl "test.spl" "foo(): Char { foo(); }" `shouldParse` FunDecl "foo" (Just CharType) [] [ExprStmt $ FunctionCall "foo" []]
                parse pFunDecl "test.spl" "foo(): [Char] { foo(); }" `shouldParse` FunDecl "foo" (Just $ ListType CharType) [] [ExprStmt $ FunctionCall "foo" []]
                parse pFunDecl "test.spl" "foo(): [a] { foo(); }" `shouldParse` FunDecl "foo" (Just $ ListType $ TypeVar "a") [] [ExprStmt $ FunctionCall "foo" []]
                parse pFunDecl "test.spl" "foo(): (a, Char) { foo(); }" `shouldParse` FunDecl "foo" (Just $ TupleType (TypeVar "a") CharType) [] [ExprStmt $ FunctionCall "foo" []]

            it "parses a function declaration with return type and type-less arguments" $ do
                parse pFunDecl "test.spl" "foo(a): Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [("a", Nothing)] [ExprStmt $ FunctionCall "foo" []]
                parse pFunDecl "test.spl" "foo(a, b): Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [("a", Nothing), ("b", Nothing)] [ExprStmt $ FunctionCall "foo" []]
                parse pFunDecl "test.spl" "foo(a, b, c): Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [("a", Nothing), ("b", Nothing), ("c", Nothing)] [ExprStmt $ FunctionCall "foo" []]
                parse pFunDecl "test.spl" "foo( a ): Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [("a", Nothing)] [ExprStmt $ FunctionCall "foo" []]

            it "parses a function declaration with return type and typed arguments" $ do
                parse pFunDecl "test.spl" "foo(a : Char): Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [("a", Just CharType)] [ExprStmt $ FunctionCall "foo" []]
                parse pFunDecl "test.spl" "foo(a : Char, b : [a]): Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [("a", Just CharType), ("b", Just $ ListType $ TypeVar "a")] [ExprStmt $ FunctionCall "foo" []]

            it "parses a function declaration with return tyep and mixed arguments" $ do
                parse pFunDecl "test.spl" "foo(a, b : [a]): Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [("a", Nothing), ("b", Just $ ListType $ TypeVar "a")] [ExprStmt $ FunctionCall "foo" []]
                parse pFunDecl "test.spl" "foo(a : Char, b): Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [("a", Just CharType), ("b", Nothing)] [ExprStmt $ FunctionCall "foo" []]

            it "doesn't parse Void arguments" $ do
                parse pFunDecl "test.spl" `shouldFailOn` "foo(a : Void): Void { foo(); }"