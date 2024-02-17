{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Program where

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)
import Parser.Program (pProgram)
import Parser.AST

programSpec :: Spec
programSpec = do
    describe "Parser.Program" $ do
        describe "pProgram" $ do
            it "parses many declarations" $ do
                parse pProgram "test.spl" "foo(): Void { foo(); } var i = 1; bar(): Void { foo(); }" `shouldParse` [FunDecl "foo" (Just VoidType) [] [ExprStmt $ FunctionCall "foo" []], VarDecl Nothing "i" (LiteralExpr $ IntLit 1), FunDecl "bar" (Just VoidType) [] [ExprStmt $ FunctionCall "foo" []]]