{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Literal where

import Test.Hspec
import Parser.Literal
import Text.Megaparsec (parse)
import Parser.AST
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)

literalSpec :: Spec
literalSpec = do
    describe "Parser.Literal" $ do
        describe "pTrue" $ do
            it "parses the string 'true' as TrueLit" $ do
                parse pTrue "test.spl" "true" `shouldParse` TrueLit

            it "doesn't parse anything else" $ do
                parse pTrue "test.spl" `shouldFailOn` "false"
                parse pTrue "test.spl" `shouldFailOn` "fa"
                parse pTrue "test.spl" `shouldFailOn` "tru"

        describe "pFalse" $ do
            it "parses the string 'true' as TrueLit" $ do
                parse pFalse "test.spl" "false" `shouldParse` FalseLit

            it "doesn't parse anything else" $ do
                parse pFalse "test.spl" `shouldFailOn` "true"
                parse pFalse "test.spl" `shouldFailOn` "tr"
                parse pFalse "test.spl" `shouldFailOn` "fals"

        describe "pEmptyList" $ do
            it "parses the empty list" $ do
                parse pEmptyList "test.spl" "[]" `shouldParse` EmptyListLit

            it "doesn't parse anything else" $ do
                parse pEmptyList "test.spl" `shouldFailOn` "[[]]"
                parse pEmptyList "test.spl" `shouldFailOn` "not_a_list"
                parse pEmptyList "test.spl" `shouldFailOn` "]["