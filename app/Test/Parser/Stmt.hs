{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Stmt where
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)
import Parser.Stmt (pStmt)
import Parser.AST
import Text.Megaparsec (parse)

-- TODO ;)


stmtSpec :: Spec
stmtSpec = do
    describe "Parser.Stmt" $ do
        describe "pStmt" $ do
            it "parses a ExpreStmt" $ do
                parse pStmt "test.spl" "3+3;" `shouldParse` ExprStmt (BinOp Add (LiteralExpr $ IntLit 3) (LiteralExpr $ IntLit 3))
                parse pStmt "test.spl" "42+137;" `shouldParse` ExprStmt (BinOp Add (LiteralExpr $ IntLit 42) (LiteralExpr $ IntLit 137))
            it "parses a ReturnStmt" $ do
                parse pStmt "test.spl" "return 10;" `shouldParse` ReturnStmt (LiteralExpr $ IntLit 10)
            it "parses a IfStmt (without else)" $ do
                parse pStmt "test.spl" "if (a > 3) a = 5;" `shouldParse` IfStmt (BinOp Gt (VariableExpr (Identifier "a")) (LiteralExpr $ IntLit 3)) (ExprStmt  $ AssignExpr (Identifier "a") (LiteralExpr $ IntLit 5)) Nothing
                parse pStmt "test.spl" "if (a == 3) a =5;" `shouldParse` IfStmt (BinOp Eq (VariableExpr (Identifier "a")) (LiteralExpr $ IntLit 3)) (ExprStmt $ AssignExpr (Identifier "a") (LiteralExpr $ IntLit 5)) Nothing
            it "parses a IfStmt (with else)" $ do
                parse pStmt "test.spl" "if (a > 3) 5; else 0;" `shouldParse` IfStmt (BinOp Gt (VariableExpr (Identifier "a")) (LiteralExpr $ IntLit 3)) (ExprStmt $ LiteralExpr (IntLit 5)) (Just (ExprStmt $ LiteralExpr (IntLit 0)))
                parse pStmt "test.spl" "if (a > 3) a = 5; else a = 0;" `shouldParse` IfStmt (BinOp Gt (VariableExpr (Identifier "a")) (LiteralExpr $ IntLit 3)) (ExprStmt $ AssignExpr (Identifier "a") (LiteralExpr $ IntLit 5)) (Just (ExprStmt $ AssignExpr (Identifier "a") (LiteralExpr $ IntLit 0)))
            --it "parses a WhileStmt" $ do
            --    parse pStmt "test.spl" "while (true) a = a + 1;" `shouldParse` WhileStmt (LiteralExpr TrueLit) (ExprStmt $ AssignExpr (Identifier "a") (BinOp Add (VariableExpr $ Identifier "a") (LiteralExpr $ IntLit 1)))