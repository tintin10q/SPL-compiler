{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Decl where

import Test.Hspec
import Text.Megaparsec (parse)
import Parser.AST
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)
import Parser.Decl (pVarDecl, pFunDecl, pDecl)

declSpec :: Spec
declSpec = do
    describe "Parser.Decl" $ do
        describe "pDecl" $ do
            it "parses a variable declaration" $ do
                parse pDecl "test.spl" "var i = 1;" `shouldParse` VarDecl Nothing "i" (LiteralExpr $ IntLit 1)
                parse pVarDecl "test.spl" "Int i = 1;" `shouldParse` VarDecl (Just IntType) "i" (LiteralExpr $ IntLit 1)

            it "parses a function declaration" $ do
                parse pFunDecl "test.spl" "foo(): Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [] [ExprStmt $ FunctionCall "foo" []]
                parse pFunDecl "test.spl" "foo(a, b : [a]): Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [("a", Nothing), ("b", Just $ ListType $ TypeVar "a")] [ExprStmt $ FunctionCall "foo" []]

        describe "pVarDecl" $ do
            it "parses a type-free variable declaration" $ do
                parse pVarDecl "test.spl" "var i = 1;" `shouldParse` VarDecl Nothing "i" (LiteralExpr $ IntLit 1)
                parse pVarDecl "test.spl" "var i = 'a';" `shouldParse` VarDecl Nothing "i" (LiteralExpr $ CharLit 'a')
                parse pVarDecl "test.spl" "var i=1;" `shouldParse` VarDecl Nothing "i" (LiteralExpr $ IntLit 1)
                parse pVarDecl "test.spl" "var i=1 ;" `shouldParse` VarDecl Nothing "i" (LiteralExpr $ IntLit 1)
                parse pVarDecl "test.spl" "var i = 1 ; " `shouldParse` VarDecl Nothing "i" (LiteralExpr $ IntLit 1)
            
            it "parses a variable declaration with variable 'var'" $ do
                parse pVarDecl "test.spl" "var var = 1;" `shouldParse` VarDecl Nothing "var" (LiteralExpr $ IntLit 1)

            it "parses a variable declaration with type" $ do
                parse pVarDecl "test.spl" "Int i = 1;" `shouldParse` VarDecl (Just IntType) "i" (LiteralExpr $ IntLit 1)
                parse pVarDecl "test.spl" "Char i = 'a';" `shouldParse` VarDecl (Just CharType) "i" (LiteralExpr $ CharLit 'a')
                parse pVarDecl "test.spl" "[Char] i = 'a':'b':[];" `shouldParse` VarDecl (Just $ ListType CharType) "i" (BinOp Cons (LiteralExpr (CharLit 'a')) (BinOp Cons (LiteralExpr (CharLit 'b')) (LiteralExpr EmptyListLit)))
                parse pVarDecl "test.spl" "(Char, Int) i = ('a', 12);" `shouldParse` VarDecl (Just $ TupleType CharType IntType) "i" (LiteralExpr $ TupleLit (LiteralExpr $ CharLit 'a', LiteralExpr $ IntLit 12))

            it "does not parse the Void return type" $ do
                parse pVarDecl "test.spl" `shouldFailOn` "Void i = 1;"

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