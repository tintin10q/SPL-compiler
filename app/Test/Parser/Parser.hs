{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Parser where

import Test.Hspec ( hspec, describe, it, shouldBe, Spec )
import Parser.Parser (pIdentifier)
import Text.Megaparsec (parse)
import Test.Parser.Literal (literalSpec)

parserSpec :: Spec
parserSpec = do
    describe "Parser.Parser" $ do
        pIdentifierSpec
        literalSpec

pIdentifierSpec :: Spec
pIdentifierSpec = do
    describe "pIdentifier" $ do
        it "parses an identifier consisting of alphabetical characters" $ do
            parse pIdentifier "test.spl" "ident" `shouldBe` Right "ident"

        it "parses an identifier with underscores" $ do
            parse pIdentifier "test.spl" "my_ident" `shouldBe` Right "my_ident"

        it "parses an identifier with one apostrophe at end" $ do
            parse pIdentifier "test.spl" "ident'" `shouldBe` Right "ident'"

        it "parses an identifier with multiple apostrophes at end" $ do
            parse pIdentifier "test.spl" "ident''''" `shouldBe` Right "ident''''"

        it "parses an identifier with multiple apostrophes at end and underscores" $ do
            parse pIdentifier "test.spl" "my_ident''''" `shouldBe` Right "my_ident''''"

        it "stops parsing after apostrophes" $ do
            parse pIdentifier "test.spl" "my_i'dent" `shouldBe` Right "my_i'"

        it "parses lower and upper-case characters" $ do
            parse pIdentifier "test.spl" "MyIdeNt" `shouldBe` Right "MyIdeNt"