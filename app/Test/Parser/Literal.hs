{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Literal where

import Test.Hspec
import Parser.Literal
import Text.Megaparsec (parse)
import Parser.AST
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)

literalSpec :: Spec
literalSpec = do
    describe "pTrue" $ do
        it "parses the string 'true' as TrueLit" $ do
            parse pTrue "test.spl" "true" `shouldParse` TrueLit

        it "doesn't parse anything else" $ do
            parse pTrue "test.spl" `shouldFailOn` "false"

    describe "pFalse" $ do
        it "parses the string 'true' as TrueLit" $ do
            parse pFalse "test.spl" "false" `shouldParse` FalseLit

        it "doesn't parse anything else" $ do
            parse pFalse "test.spl" `shouldFailOn` "true"