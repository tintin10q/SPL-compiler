{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Parser where

import Test.Parser.Expr (exprSpec)
import Test.Parser.Lexer (lexerSpec)
import Test.Hspec (Spec, describe)
import Test.Parser.Stmt (stmtSpec)


parserSpec :: Spec
parserSpec = do
    describe "Parser.Parser" $ do
        lexerSpec
        exprSpec
        stmtSpec
