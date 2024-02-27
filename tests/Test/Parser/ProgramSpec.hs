{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.ProgramSpec (programSpec) where

import SPL.Parser.AST
import SPL.Parser.Program (pProgram)

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)

programSpec :: Spec
programSpec = do
    describe "Parser.Program" $ do
        describe "pProgram" $ do
            it "parses many declarations" $ do
                parse pProgram "test.spl" "foo(): Void { foo(); } bar(): Void { foo(); }" `shouldParse` [FunDecl "foo" (Just VoidType) [] [ExprStmt $ FunctionCall "foo" []], FunDecl "bar" (Just VoidType) [] [ExprStmt $ FunctionCall "foo" []]]