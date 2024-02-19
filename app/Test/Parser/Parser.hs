{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Parser where

import Test.Parser.Decl (declSpec)
import Test.Parser.Expr (exprSpec)
import Test.Parser.Lexer (lexerSpec)
import Test.Parser.Program (programSpec)
import Test.Parser.Stmt (stmtSpec)
import Test.Parser.Type (typeSpec)
import Test.Hspec (Spec, describe)

parserSpec :: Spec
parserSpec = do
    describe "Parser.Parser" $ do
        declSpec
        exprSpec
        lexerSpec
        programSpec
        stmtSpec
        typeSpec
