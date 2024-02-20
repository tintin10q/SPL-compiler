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
        describe "pExpr" $ do
            it "parses a term" $ do
                parse pExpr "test.spl" "1" `shouldParse` LiteralExpr (IntLit 1)
                parse pExpr "test.spl" "a = 'b'" `shouldParse` AssignExpr (Identifier "a" Nothing) (LiteralExpr $ CharLit 'b')
                parse pExpr "test.spl" "ident" `shouldParse` VariableExpr (Identifier "ident" Nothing)
                parse pExpr "test.spl" "(a + b)" `shouldParse` BinOp Add (VariableExpr (Identifier "a" Nothing)) (VariableExpr (Identifier "b" Nothing))

            it "parses a simple unary expression" $ do
                parse pExpr "test.spl" "!true" `shouldParse` UnaryOp Negate (LiteralExpr TrueLit)
                parse pExpr "test.spl" "!'a'" `shouldParse` UnaryOp Negate (LiteralExpr $ CharLit 'a')
                parse pExpr "test.spl" "!a" `shouldParse` UnaryOp Negate (VariableExpr (Identifier "a" Nothing))
                parse pExpr "test.spl" "!a = 'b'" `shouldParse` UnaryOp Negate (AssignExpr (Identifier "a" Nothing) (LiteralExpr $ CharLit 'b'))
                parse pExpr "test.spl" "(1, 2).hd" `shouldParse` UnaryOp (FieldAccess HeadField) (LiteralExpr $ TupleLit (LiteralExpr $ IntLit 1, LiteralExpr $ IntLit 2))
                parse pExpr "test.spl" "(1, 2).tl" `shouldParse` UnaryOp (FieldAccess TailField) (LiteralExpr $ TupleLit (LiteralExpr $ IntLit 1, LiteralExpr $ IntLit 2))

            it "parses a simple binary expression" $ do
                parse pExpr "test.spl" "1 + 2" `shouldParse` BinOp Add (LiteralExpr $ IntLit 1) (LiteralExpr $ IntLit 2)
                parse pExpr "test.spl" "a + b" `shouldParse` BinOp Add (VariableExpr $ Identifier "a" Nothing) (VariableExpr $ Identifier "b" Nothing)
                parse pExpr "test.spl" "f(a + b) + c" `shouldParse` BinOp Add (FunctionCall "f" [BinOp Add (VariableExpr (Identifier "a" Nothing)) (VariableExpr (Identifier "b" Nothing))]) (VariableExpr (Identifier "c" Nothing))

            it "parses an expression" $ do
                parse pExpr "test.spl" "1 + 2 * 4" `shouldParse` BinOp Add (LiteralExpr $ IntLit 1) (BinOp Mul (LiteralExpr $ IntLit 2) (LiteralExpr $ IntLit 4))
                parse pExpr "test.spl" "1 * (2 + 4) * 16" `shouldParse` BinOp Mul (BinOp Mul (LiteralExpr (IntLit 1)) (BinOp Add (LiteralExpr (IntLit 2)) (LiteralExpr (IntLit 4)))) (LiteralExpr (IntLit 16))
                parse pExpr "test.spl" "1 + 2 * 3 / 4" `shouldParse` BinOp Add (LiteralExpr (IntLit 1)) (BinOp Div (BinOp Mul (LiteralExpr (IntLit 2)) (LiteralExpr (IntLit 3))) (LiteralExpr (IntLit 4)))
                parse pExpr "test.spl" "(1, 2).tl + 3" `shouldParse` BinOp Add (UnaryOp (FieldAccess TailField) (LiteralExpr $ TupleLit (LiteralExpr $ IntLit 1, LiteralExpr $ IntLit 2))) (LiteralExpr $ IntLit 3)
                parse pExpr "test.spl" "a.tl + b.hd" `shouldParse` BinOp Add (VariableExpr (Identifier "a" (Just TailField))) (VariableExpr (Identifier "b" (Just HeadField)))

        describe "pAssignExpr" $ do
            it "parses an assignment" $ do
                parse pAssignExpr "test.spl" "a = 'b'" `shouldParse` AssignExpr (Identifier "a" Nothing) (LiteralExpr $ CharLit 'b')
                parse pAssignExpr "test.spl" "foobar = 'b'" `shouldParse` AssignExpr (Identifier "foobar" Nothing) (LiteralExpr $ CharLit 'b')
                parse pAssignExpr "test.spl" "a'' = 'b'" `shouldParse` AssignExpr (Identifier "a''" Nothing) (LiteralExpr $ CharLit 'b')
                parse pAssignExpr "test.spl" "a = true" `shouldParse` AssignExpr (Identifier "a" Nothing) (LiteralExpr TrueLit)
                parse pAssignExpr "test.spl" "a = false" `shouldParse` AssignExpr (Identifier "a" Nothing) (LiteralExpr FalseLit)
                parse pAssignExpr "test.spl" "a = ('a', 12.0)" `shouldParse` AssignExpr (Identifier "a" Nothing) (LiteralExpr $ TupleLit (LiteralExpr $ CharLit 'a', LiteralExpr $ FloatLit 12.0))

            it "parses nested assigments" $ do
                parse pAssignExpr "test.spl" "a = b = 'c'" `shouldParse` AssignExpr (Identifier "a" Nothing) (AssignExpr (Identifier "b" Nothing) (LiteralExpr $ CharLit 'c'))
                parse pAssignExpr "test.spl" "a = b = c = 'd'" `shouldParse` AssignExpr (Identifier "a" Nothing) (AssignExpr (Identifier "b" Nothing) (AssignExpr (Identifier "c" Nothing) (LiteralExpr $ CharLit 'd')))
                parse pAssignExpr "test.spl" "a = (a = 'b', 'c')" `shouldParse` AssignExpr (Identifier "a" Nothing) (LiteralExpr $ TupleLit (AssignExpr (Identifier "a" Nothing) (LiteralExpr $ CharLit 'b'), LiteralExpr $ CharLit 'c'))

            it "handles whitespace correctly" $ do
                parse pAssignExpr "test.spl" "a='b'" `shouldParse` AssignExpr (Identifier "a" Nothing) (LiteralExpr $ CharLit 'b')
                parse pAssignExpr "test.spl" "a='b'  " `shouldParse` AssignExpr (Identifier "a" Nothing) (LiteralExpr $ CharLit 'b')
                parse pAssignExpr "test.spl" "a  =  'b'" `shouldParse` AssignExpr (Identifier "a" Nothing) (LiteralExpr $ CharLit 'b')
                parse pAssignExpr "test.spl" "a=    'b'" `shouldParse` AssignExpr (Identifier "a" Nothing) (LiteralExpr $ CharLit 'b')

        describe "pFunctionCall" $ do
            it "parses an empty function call" $ do
                parse pFunctionCall "test.spl" "f()" `shouldParse` FunctionCall "f" []
                parse pFunctionCall "test.spl" "test()" `shouldParse` FunctionCall "test" []
                parse pFunctionCall "test.spl" "while()" `shouldParse` FunctionCall "while" []
                parse pFunctionCall "test.spl" "if()" `shouldParse` FunctionCall "if" []
                parse pFunctionCall "test.spl" "f ()" `shouldParse` FunctionCall "f" []
                parse pFunctionCall "test.spl" "f(   )" `shouldParse` FunctionCall "f" []
                parse pFunctionCall "test.spl" "f (   )" `shouldParse` FunctionCall "f" []
                parse pFunctionCall "test.spl" "f()  " `shouldParse` FunctionCall "f" []

            it "parses a function call with single argument" $ do
                parse pFunctionCall "test.spl" "f('a')" `shouldParse` FunctionCall "f" [LiteralExpr $ CharLit 'a']
                parse pFunctionCall "test.spl" "f  ('a')" `shouldParse` FunctionCall "f" [LiteralExpr $ CharLit 'a']
                parse pFunctionCall "test.spl" "f(  'a'  )" `shouldParse` FunctionCall "f" [LiteralExpr $ CharLit 'a']
                parse pFunctionCall "test.spl" "f  (  'a'  )" `shouldParse` FunctionCall "f" [LiteralExpr $ CharLit 'a']
                parse pFunctionCall "test.spl" "f(f('a'))" `shouldParse` FunctionCall "f" [FunctionCall "f" [LiteralExpr $ CharLit 'a']]
                parse pFunctionCall "test.spl" "f(f(1 + 348 + 19))" `shouldParse` FunctionCall "f" [FunctionCall "f" [BinOp Add (BinOp Add (LiteralExpr (IntLit 1)) (LiteralExpr (IntLit 348))) (LiteralExpr (IntLit 19))]]
                parse pFunctionCall "test.spl" "f(12)" `shouldParse` FunctionCall "f" [LiteralExpr $ IntLit 12]
                parse pFunctionCall "test.spl" "f(2 * (1 + 3))" `shouldParse` FunctionCall "f" [BinOp Mul (LiteralExpr (IntLit 2)) (BinOp Add (LiteralExpr (IntLit 1)) (LiteralExpr (IntLit 3)))]
                parse pFunctionCall "test.spl" "f((1 + 3) * 2)" `shouldParse` FunctionCall "f" [BinOp Mul (BinOp Add (LiteralExpr (IntLit 1)) (LiteralExpr (IntLit 3))) (LiteralExpr (IntLit 2))]

            it "parses a function call with multiple arguments" $ do
                parse pFunctionCall "test.spl" "f('a', 'b')" `shouldParse` FunctionCall "f" [LiteralExpr $ CharLit 'a', LiteralExpr $ CharLit 'b']
                parse pFunctionCall "test.spl" "f  ('a', 'b')" `shouldParse` FunctionCall "f" [LiteralExpr $ CharLit 'a', LiteralExpr $ CharLit 'b']
                parse pFunctionCall "test.spl" "f(  'a', 'b'  )" `shouldParse` FunctionCall "f" [LiteralExpr $ CharLit 'a', LiteralExpr $ CharLit 'b']
                parse pFunctionCall "test.spl" "f  (  'a', 'b'  )" `shouldParse` FunctionCall "f" [LiteralExpr $ CharLit 'a', LiteralExpr $ CharLit 'b']
                parse pFunctionCall "test.spl" "f('a'  , 'b')" `shouldParse` FunctionCall "f" [LiteralExpr $ CharLit 'a', LiteralExpr $ CharLit 'b']
                parse pFunctionCall "test.spl" "f('a','b')" `shouldParse` FunctionCall "f" [LiteralExpr $ CharLit 'a', LiteralExpr $ CharLit 'b']
                parse pFunctionCall "test.spl" "f(f(), f())" `shouldParse` FunctionCall "f" [FunctionCall "f" [], FunctionCall "f" []]
                parse pFunctionCall "test.spl" "f(f(f()), f())" `shouldParse` FunctionCall "f" [FunctionCall "f" [FunctionCall "f" []], FunctionCall "f" []]
                parse pFunctionCall "test.spl" "f(1.2 + 23.1, f(1) + (12 + 39), 'a')" `shouldParse` FunctionCall "f" [BinOp Add (LiteralExpr (FloatLit 1.2)) (LiteralExpr (FloatLit 23.1)), BinOp Add (FunctionCall "f" [LiteralExpr (IntLit 1)]) (BinOp Add (LiteralExpr (IntLit 12)) (LiteralExpr (IntLit 39))), LiteralExpr (CharLit 'a')]

            it "parses a function call with tuple arguments" $ do
                parse pFunctionCall "test.spl" "f((1, 2))" `shouldParse` FunctionCall "f" [LiteralExpr $ TupleLit (LiteralExpr (IntLit 1), LiteralExpr (IntLit 2))]
                parse pFunctionCall "test.spl" "f((1, 2), 3)" `shouldParse` FunctionCall "f" [LiteralExpr $ TupleLit (LiteralExpr (IntLit 1), LiteralExpr (IntLit 2)), LiteralExpr $ IntLit 3]

        describe "pVariableExpr" $ do
            it "parses a simple identifier variable" $ do
                parse pVariableExpr "test.spl" "ident" `shouldParse` VariableExpr (Identifier "ident" Nothing)
                parse pVariableExpr "test.spl" "iDent" `shouldParse` VariableExpr (Identifier "iDent" Nothing)
                parse pVariableExpr "test.spl" "iDent'''" `shouldParse` VariableExpr (Identifier "iDent'''" Nothing)

            it "parses head access" $ do
                parse pVariableExpr "test.spl" "a.hd" `shouldParse` VariableExpr (Identifier "a" (Just HeadField))
                parse pVariableExpr "test.spl" "hd.hd" `shouldParse` VariableExpr (Identifier "hd" (Just HeadField))
                parse pVariableExpr "test.spl" "hd  .  hd" `shouldParse` VariableExpr (Identifier "hd" (Just HeadField))

            it "parses tail access" $ do
                parse pVariableExpr "test.spl" "a.tl" `shouldParse` VariableExpr (Identifier "a" (Just TailField))
                parse pVariableExpr "test.spl" "tl.tl" `shouldParse` VariableExpr (Identifier "tl" (Just TailField))
                parse pVariableExpr "test.spl" "tl  .  tl" `shouldParse` VariableExpr (Identifier "tl" (Just TailField))

            it "discards trailing whitespace" $ do
                parse pVariableExpr "test.spl" "ident   " `shouldParse` VariableExpr (Identifier "ident" Nothing)

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
