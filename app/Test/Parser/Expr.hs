{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Expr where

import Test.Hspec
import Text.Megaparsec (parse)
import Parser.AST
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)
import Parser.Expr

exprSpec :: Spec
exprSpec = do
    describe "Parser.Expr" $ do
        literalSpec

        describe "pExpr" $ do
            it "parses an expression" $ do
                parse pExpr "test.spl" "1 + 2 * 4" `shouldParse` BinOp Add (LiteralExpr $ IntLit 1) (BinOp Mul (LiteralExpr $ IntLit 2) (LiteralExpr $ IntLit 3))

literalSpec :: Spec
literalSpec = do
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

    describe "pInt" $ do
        it "parses an integer" $ do
            parse pInt "test.spl" "123" `shouldParse` IntLit 123
            parse pInt "test.spl" "827" `shouldParse` IntLit 827

        it "parses a signed integer" $ do
            parse pInt "test.spl" "-123" `shouldParse` IntLit (-123)
            parse pInt "test.spl" "-827" `shouldParse` IntLit (-827)
            parse pInt "test.spl" "+123" `shouldParse` IntLit 123
            parse pInt "test.spl" "+827" `shouldParse` IntLit 827
            parse pInt "test.spl" "-  123" `shouldParse` IntLit (-123)
            parse pInt "test.spl" "-  827" `shouldParse` IntLit (-827)
            parse pInt "test.spl" "+  123" `shouldParse` IntLit 123
            parse pInt "test.spl" "+  827" `shouldParse` IntLit 827

        it "discards trailing whitespace" $ do
            parse pInt "test.spl" "123   " `shouldParse` IntLit 123

    describe "pFloat" $ do
        it "parses a float" $ do
            parse pFloat "test.spl" "1.0" `shouldParse` FloatLit 1.0
            parse pFloat "test.spl" "12.12" `shouldParse` FloatLit 12.12
            parse pFloat "test.spl" "420.000001" `shouldParse` FloatLit 420.000001
            parse pFloat "test.spl" "69420.0" `shouldParse` FloatLit 69420.0
            parse pFloat "test.spl" "0.1" `shouldParse` FloatLit 0.1

        it "parses a signed float" $ do
            parse pFloat "test.spl" "-1.0" `shouldParse` FloatLit (-1.0)
            parse pFloat "test.spl" "+1.0" `shouldParse` FloatLit 1.0
            parse pFloat "test.spl" "-   1.0" `shouldParse` FloatLit (-1.0)
            parse pFloat "test.spl" "+   1.0" `shouldParse` FloatLit 1.0

        it "discards trailing whitespace" $ do
            parse pFloat "test.spl" "1.0   " `shouldParse` FloatLit 1.0

    describe "pChar" $ do
        it "parses a simple character" $ do
            parse pChar "test.spl" "'a'" `shouldParse` CharLit 'a'
            parse pChar "test.spl" "'1'" `shouldParse` CharLit '1'
            parse pChar "test.spl" "'*'" `shouldParse` CharLit '*'
            parse pChar "test.spl" "' '" `shouldParse` CharLit ' '
        
        it "parses an escaped character" $ do
            parse pChar "test.spl" "'\\n'" `shouldParse` CharLit '\n'
            parse pChar "test.spl" "'\\t'" `shouldParse` CharLit '\t'
            parse pChar "test.spl" "'\\\\'" `shouldParse` CharLit '\\'

        it "parses an escaped single quote" $ do
            parse pChar "test.spl" "'\\''" `shouldParse` CharLit '\''

        it "parses unicode characters" $ do
            parse pChar "test.spl" "'😀'" `shouldParse` CharLit '😀'

        it "discards trailing whitespace" $ do
            parse pChar "test.spl" "'a'   " `shouldParse` CharLit 'a'

        it "does not parse multiple characters" $ do
            parse pChar "test.spl" `shouldFailOn` "'aa'"
            parse pChar "test.spl" `shouldFailOn` "'ab'"
            parse pChar "test.spl" `shouldFailOn` "' a'"
            parse pChar "test.spl" `shouldFailOn` "'a '"
            parse pChar "test.spl" `shouldFailOn` "'  '"

    describe "pTuple" $ do
        it "parses a tuple of two expressions" $ do
            parse pTuple "test.spl" "('a', 12.0)" `shouldParse` TupleLit (LiteralExpr $ CharLit 'a', LiteralExpr $ FloatLit 12.0)

    describe "pEmptyList" $ do
        it "parses the empty list" $ do
            parse pEmptyList "test.spl" "[]" `shouldParse` EmptyListLit

        it "doesn't parse anything else" $ do
            parse pEmptyList "test.spl" `shouldFailOn` "[[]]"
            parse pEmptyList "test.spl" `shouldFailOn` "not_a_list"
            parse pEmptyList "test.spl" `shouldFailOn` "]["

        it "discards trailing whitespace" $ do
            parse pEmptyList "test.spl" "[]    " `shouldParse` EmptyListLit