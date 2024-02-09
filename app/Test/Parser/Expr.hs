{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Expr where

import Test.Hspec
import Text.Megaparsec (parse)
import Parser.AST
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)
import Parser.Expr
    ( pAssignExpr,
      pChar,
      pEmptyList,
      pExpr,
      pFalse,
      pFloat,
      pInt,
      pLiteralExpr,
      pTrue,
      pTuple,
      pVariableExpr )

exprSpec :: Spec
exprSpec = do
    describe "Parser.Expr" $ do
        describe "pExpr" $ do
            it "parses a term" $ do
                parse pExpr "test.spl" "1" `shouldParse` LiteralExpr (IntLit 1)
                parse pExpr "test.spl" "a = 'b'" `shouldParse` AssignExpr (Identifier "a") (LiteralExpr $ CharLit 'b')
                parse pExpr "test.spl" "ident" `shouldParse` VariableExpr (Identifier "ident")

            it "parses a simple unary expression" $ do
                parse pExpr "test.spl" "!true" `shouldParse` UnaryOp Negate (LiteralExpr TrueLit)
                parse pExpr "test.spl" "!'a'" `shouldParse` UnaryOp Negate (LiteralExpr $ CharLit 'a')
                parse pExpr "test.spl" "!a" `shouldParse` UnaryOp Negate (VariableExpr (Identifier "a"))
                parse pExpr "test.spl" "!a = 'b'" `shouldParse` UnaryOp Negate (AssignExpr (Identifier "a") (LiteralExpr $ CharLit 'b'))

            it "parses a simple binary expression" $ do
                parse pExpr "test.spl" "1 + 2" `shouldParse` BinOp Add (LiteralExpr $ IntLit 1) (LiteralExpr $ IntLit 2)
                parse pExpr "test.spl" "a + b" `shouldParse` BinOp Add (VariableExpr $ Identifier "a") (VariableExpr $ Identifier "b")

            it "parses an expression" $ do
                parse pExpr "test.spl" "1 + 2 * 4" `shouldParse` BinOp Add (LiteralExpr $ IntLit 1) (BinOp Mul (LiteralExpr $ IntLit 2) (LiteralExpr $ IntLit 4))

        describe "pAssignExpr" $ do
            it "parses an assignment" $ do
                parse pAssignExpr "test.spl" "a = 'b'" `shouldParse` AssignExpr (Identifier "a") (LiteralExpr $ CharLit 'b')
                parse pAssignExpr "test.spl" "foobar = 'b'" `shouldParse` AssignExpr (Identifier "foobar") (LiteralExpr $ CharLit 'b')
                parse pAssignExpr "test.spl" "a'' = 'b'" `shouldParse` AssignExpr (Identifier "a''") (LiteralExpr $ CharLit 'b')
                parse pAssignExpr "test.spl" "a = true" `shouldParse` AssignExpr (Identifier "a") (LiteralExpr TrueLit)
                parse pAssignExpr "test.spl" "a = false" `shouldParse` AssignExpr (Identifier "a") (LiteralExpr FalseLit)
                parse pAssignExpr "test.spl" "a = ('a', 12.0)" `shouldParse` AssignExpr (Identifier "a") (LiteralExpr $ TupleLit (LiteralExpr $ CharLit 'a', LiteralExpr $ FloatLit 12.0))

            it "parses nested assigments" $ do
                parse pAssignExpr "test.spl" "a = b = 'c'" `shouldParse` AssignExpr (Identifier "a") (AssignExpr (Identifier "b") (LiteralExpr $ CharLit 'c'))
                parse pAssignExpr "test.spl" "a = b = c = 'd'" `shouldParse` AssignExpr (Identifier "a") (AssignExpr (Identifier "b") (AssignExpr (Identifier "c") (LiteralExpr $ CharLit 'd')))
                parse pAssignExpr "test.spl" "a = (a = 'b', 'c')" `shouldParse` AssignExpr (Identifier "a") (LiteralExpr $ TupleLit (AssignExpr (Identifier "a") (LiteralExpr $ CharLit 'b'), LiteralExpr $ CharLit 'c'))

            it "handles whitespace correctly" $ do
                parse pAssignExpr "test.spl" "a='b'" `shouldParse` AssignExpr (Identifier "a") (LiteralExpr $ CharLit 'b')
                parse pAssignExpr "test.spl" "a='b'  " `shouldParse` AssignExpr (Identifier "a") (LiteralExpr $ CharLit 'b')
                parse pAssignExpr "test.spl" "a  =  'b'" `shouldParse` AssignExpr (Identifier "a") (LiteralExpr $ CharLit 'b')
                parse pAssignExpr "test.spl" "a=    'b'" `shouldParse` AssignExpr (Identifier "a") (LiteralExpr $ CharLit 'b')

        describe "pVariableExpr" $ do
            it "parses a simple identifier variable" $ do
                parse pVariableExpr "test.spl" "ident" `shouldParse` VariableExpr (Identifier "ident")
                parse pVariableExpr "test.spl" "iDent" `shouldParse` VariableExpr (Identifier "iDent")
                parse pVariableExpr "test.spl" "iDent'''" `shouldParse` VariableExpr (Identifier "iDent'''")

            it "discards trailing whitespace" $ do
                parse pVariableExpr "test.spl" "ident   " `shouldParse` VariableExpr (Identifier "ident")

        describe "pLiteralExpr" $ do
            it "parses a literal" $ do
                parse pLiteralExpr "test.spl" "true" `shouldParse` LiteralExpr TrueLit
                parse pLiteralExpr "test.spl" "false" `shouldParse` LiteralExpr FalseLit
                parse pLiteralExpr "test.spl" "123" `shouldParse` LiteralExpr (IntLit 123)
                parse pLiteralExpr "test.spl" "-123" `shouldParse` LiteralExpr (IntLit (-123))
                parse pLiteralExpr "test.spl" "+123" `shouldParse` LiteralExpr (IntLit 123)
                parse pLiteralExpr "test.spl" "1.0" `shouldParse` LiteralExpr (FloatLit 1.0)
                parse pLiteralExpr "test.spl" "-1.0" `shouldParse` LiteralExpr (FloatLit (-1.0))
                parse pLiteralExpr "test.spl" "+1.0" `shouldParse` LiteralExpr (FloatLit 1.0)
                parse pLiteralExpr "test.spl" "'a'" `shouldParse` LiteralExpr (CharLit 'a')
                parse pLiteralExpr "test.spl" "('a', 12.0)" `shouldParse` LiteralExpr (TupleLit (LiteralExpr $ CharLit 'a', LiteralExpr $ FloatLit 12.0))
                parse pLiteralExpr "test.spl" "[]" `shouldParse` LiteralExpr EmptyListLit

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