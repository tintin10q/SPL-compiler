{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Test.Printer where

import Parser.Program
import Parser.AST
import Text.Megaparsec (parse, errorBundlePretty)

program :: String
program = "fac(n : Int) : Int { \
\    if (n == 0) {\
\        return 1;\
\    }\
\\
\    return n * fac(n - 1);\
\}\
\\
\fac(n : Int) : Int {\
\    if (n == 0) {\
\        return 1;\
\    }\
\\
\    return n * fac(n - 1);\
\}"


parseProgram :: String -> Program
parseProgram p = parse pProgram p 

-- describe "Printer" $ do
            -- it "prints a complicated program" $ do
                -- parse pDecl "test.spl" "foo(): Void { foo(); }" `shouldParse` FunDecl "foo" (Just VoidType) [] [ExprStmt $ FunctionCall "foo" []]
                -- parse pDecl "