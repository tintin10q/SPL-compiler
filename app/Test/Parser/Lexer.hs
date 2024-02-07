{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Lexer where

import Test.Hspec (describe, it, Spec)
import Text.Megaparsec (parse)
import Parser.Lexer (tIdentifier)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)

lexerSpec :: Spec
lexerSpec = do
    describe "tIdentifier" $ do
        it "parses an identifier consisting of alphabetical characters" $ do
            parse tIdentifier "test.spl" "ident" `shouldParse` "ident"

        it "parses an identifier with underscores" $ do
            parse tIdentifier "test.spl" "my_ident" `shouldParse` "my_ident"

        it "parses an identifier with one apostrophe at end" $ do
            parse tIdentifier "test.spl" "ident'" `shouldParse` "ident'"

        it "parses an identifier with multiple apostrophes at end" $ do
            parse tIdentifier "test.spl" "ident''''" `shouldParse` "ident''''"

        it "parses an identifier with multiple apostrophes at end and underscores" $ do
            parse tIdentifier "test.spl" "my_ident''''" `shouldParse` "my_ident''''"

        it "stops parsing after apostrophes" $ do
            parse tIdentifier "test.spl" "my_i'dent" `shouldParse` "my_i'"

        it "parses lower and upper-case characters" $ do
            parse tIdentifier "test.spl" "MyIdeNt" `shouldParse` "MyIdeNt"

        it "doesn't parse identifiers starting with a digit" $ do
            parse tIdentifier "test.spl" `shouldFailOn` "1dent"
