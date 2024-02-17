{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Stmt where
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Parser.Stmt
import Parser.AST
import Text.Megaparsec (parse)


stmtSpec :: Spec
stmtSpec = do
    describe "Parser.Stmt" $ do
        describe "pStmt" $ do
            it "parses a expression statement" $ do
                parse pStmt "test.spl" "3+3;" `shouldParse` ExprStmt (BinOp Add (LiteralExpr $ IntLit 3) (LiteralExpr $ IntLit 3))
                parse pStmt "test.spl" "42+137;" `shouldParse` ExprStmt (BinOp Add (LiteralExpr $ IntLit 42) (LiteralExpr $ IntLit 137))
            it "parses a return statement" $ do
                parse pStmt "test.spl" "return;" `shouldParse` ReturnStmt Nothing
                parse pStmt "test.spl" "return 10;" `shouldParse` ReturnStmt (Just $ LiteralExpr $ IntLit 10)
            it "parses an if statement" $ do
                parse pStmt "test.spl" "if (true) {f();}" `shouldParse` IfStmt (LiteralExpr TrueLit) [ExprStmt $ FunctionCall "f" []] Nothing
                parse pStmt "test.spl" "if (true) { a(); } else { b(); }" `shouldParse` IfStmt (LiteralExpr TrueLit) [ExprStmt $ FunctionCall "a" []] (Just [ExprStmt $ FunctionCall "b" []])
            it "parses a while statement" $ do
                parse pStmt "test.spl" "while(true) {a(); b(); c();}" `shouldParse` WhileStmt (LiteralExpr TrueLit) [ExprStmt $ FunctionCall "a" [], ExprStmt $ FunctionCall "b" [], ExprStmt $ FunctionCall "c" []]
    
        describe "pIfStmt" $ do
            it "parses an empty if statement" $ do
                parse pIfStmt "test.spl" "if (true) {}" `shouldParse` IfStmt (LiteralExpr TrueLit) [] Nothing
                parse pIfStmt "test.spl" "if ( true ) {}" `shouldParse` IfStmt (LiteralExpr TrueLit) [] Nothing
                parse pIfStmt "test.spl" "if(true) { }" `shouldParse` IfStmt (LiteralExpr TrueLit) [] Nothing
                parse pIfStmt "test.spl" "if ( true ) {  }" `shouldParse` IfStmt (LiteralExpr TrueLit) [] Nothing

            it "parses an empty if statement with alternative" $ do
                parse pIfStmt "test.spl" "if (true) {} else {}" `shouldParse` IfStmt (LiteralExpr TrueLit) [] (Just [])
                parse pIfStmt "test.spl" "if ( true ) {} else { }" `shouldParse` IfStmt (LiteralExpr TrueLit) [] (Just [])
                parse pIfStmt "test.spl" "if(true) { }else{}" `shouldParse` IfStmt (LiteralExpr TrueLit) [] (Just [])
                parse pIfStmt "test.spl" "if ( true ) {  }   else {    }" `shouldParse` IfStmt (LiteralExpr TrueLit) [] (Just [])

            it "parses an if statement" $ do
                parse pIfStmt "test.spl" "if (true) {f();}" `shouldParse` IfStmt (LiteralExpr TrueLit) [ExprStmt $ FunctionCall "f" []] Nothing
                parse pIfStmt "test.spl" "if (true) {if();}" `shouldParse` IfStmt (LiteralExpr TrueLit) [ExprStmt $ FunctionCall "if" []] Nothing
                parse pIfStmt "test.spl" "if (true) {a(); b();}" `shouldParse` IfStmt (LiteralExpr TrueLit) [ExprStmt $ FunctionCall "a" [], ExprStmt $ FunctionCall "b" []] Nothing

            it "parses an if statement with alternative" $ do
                parse pIfStmt "test.spl" "if (true) { a(); } else { b(); }" `shouldParse` IfStmt (LiteralExpr TrueLit) [ExprStmt $ FunctionCall "a" []] (Just [ExprStmt $ FunctionCall "b" []])

        describe "pWhileStmt" $ do
            it "parses an empty while statement" $ do
                parse pWhileStmt "test.spl" "while (true) {}" `shouldParse` WhileStmt (LiteralExpr TrueLit) []
                parse pWhileStmt "test.spl" "while (true) {  }" `shouldParse` WhileStmt (LiteralExpr TrueLit) []
                parse pWhileStmt "test.spl" "while ( true ) {}" `shouldParse` WhileStmt (LiteralExpr TrueLit) []
                parse pWhileStmt "test.spl" "while(true) {}" `shouldParse` WhileStmt (LiteralExpr TrueLit) []

            it "parses a simple while statement" $ do
                parse pWhileStmt "test.spl" "while(true) {f();}" `shouldParse` WhileStmt (LiteralExpr TrueLit) [ExprStmt $ FunctionCall "f" []]
                parse pWhileStmt "test.spl" "while(true) {a(); b(); c();}" `shouldParse` WhileStmt (LiteralExpr TrueLit) [ExprStmt $ FunctionCall "a" [], ExprStmt $ FunctionCall "b" [], ExprStmt $ FunctionCall "c" []]

            it "parses a nested while statement" $ do
                parse pWhileStmt "test.spl" "while(true) { while(false) { } }" `shouldParse` WhileStmt (LiteralExpr TrueLit) [WhileStmt (LiteralExpr FalseLit) []]
                parse pWhileStmt "test.spl" "while(true) { f(); while(false) { } }" `shouldParse` WhileStmt (LiteralExpr TrueLit) [ExprStmt $ FunctionCall "f" [], WhileStmt (LiteralExpr FalseLit) []]

        describe "pExprStmt" $ do
            it "parses an assignment" $ do
                parse pExprStmt "test.spl" "a = 12;" `shouldParse` ExprStmt (AssignExpr (Identifier "a") (LiteralExpr $ IntLit 12))
                parse pExprStmt "test.spl" "a = (1, 2);" `shouldParse` ExprStmt (AssignExpr (Identifier "a") (LiteralExpr $ TupleLit (LiteralExpr $ IntLit 1, LiteralExpr $ IntLit 2)))

            it "parses a function call" $ do
                parse pExprStmt "test.spl" "f();" `shouldParse` ExprStmt (FunctionCall "f" [])
                parse pExprStmt "test.spl" "f('a', 'b');" `shouldParse` ExprStmt (FunctionCall "f" [LiteralExpr $ CharLit 'a', LiteralExpr $ CharLit 'b'])

        describe "pReturnStmt" $ do
            it "parses a return statement without a value" $ do
                parse pReturnStmt "test.spl" "return;" `shouldParse` ReturnStmt Nothing
                parse pReturnStmt "test.spl" "return  ;" `shouldParse` ReturnStmt Nothing
                parse pReturnStmt "test.spl" "return;  " `shouldParse` ReturnStmt Nothing
                parse pReturnStmt "test.spl" "return  ;  " `shouldParse` ReturnStmt Nothing

            it "parses a return statement with a value" $ do
                parse pReturnStmt "test.spl" "return 1;" `shouldParse` ReturnStmt (Just $ LiteralExpr $ IntLit 1)
                parse pReturnStmt "test.spl" "return 1 + 1;" `shouldParse` ReturnStmt (Just $ BinOp Add (LiteralExpr $ IntLit 1) (LiteralExpr $ IntLit 1))
                parse pReturnStmt "test.spl" "return f() ;" `shouldParse` ReturnStmt (Just $ FunctionCall "f" [])
                parse pReturnStmt "test.spl" "return (1 , 2);" `shouldParse` ReturnStmt (Just $ LiteralExpr $ TupleLit (LiteralExpr (IntLit 1), LiteralExpr (IntLit 2)))