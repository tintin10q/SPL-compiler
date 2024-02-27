{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.ParserSpec (parserSpec) where

import Test.Parser.DeclSpec (declSpec)
import Test.Parser.ExprSpec (exprSpec)
import Test.Parser.LexerSpec (lexerSpec)
import Test.Parser.ProgramSpec (programSpec)
import Test.Parser.StmtSpec (stmtSpec)
import Test.Parser.TypeSpec (typeSpec)

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
