{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.AST where


import Parser.AST
import Test.Hspec (Spec, describe, it, shouldBe)


astSpec :: Spec
astSpec = do describe "Parser.AST" $ do 
                describe "instance Semigroup Stmt where" $ do 
                    it "tests Stmt is a semigroup" $ do
                        BlockStmt [ExprStmt (LiteralExpr TrueLit)] <> BlockStmt [ReturnStmt (LiteralExpr TrueLit)] `shouldBe` BlockStmt [ExprStmt (LiteralExpr TrueLit), ReturnStmt (LiteralExpr TrueLit)]
                        BlockStmt [ExprStmt (LiteralExpr TrueLit)] <> ReturnStmt (LiteralExpr TrueLit) `shouldBe` BlockStmt [ExprStmt (LiteralExpr TrueLit), ReturnStmt (LiteralExpr TrueLit)]
                        ExprStmt (LiteralExpr TrueLit) <> BlockStmt [ReturnStmt (LiteralExpr TrueLit)] `shouldBe` BlockStmt [ExprStmt (LiteralExpr TrueLit), ReturnStmt (LiteralExpr TrueLit)]
                        ExprStmt (LiteralExpr TrueLit) <> ReturnStmt (LiteralExpr TrueLit) `shouldBe` BlockStmt [ExprStmt (LiteralExpr TrueLit), ReturnStmt (LiteralExpr TrueLit)]

