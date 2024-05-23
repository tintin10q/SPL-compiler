{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import SPL.Parser
-- import SPL.Printer
import SPL.Colors
import System.Environment (getArgs)

import qualified Data.Text.IO as TIO
import SPL.AST
import SPL.Typechecker2
import qualified Debug.Trace as Debug

main :: IO ()
main = do
    file:_ <- getArgs
    source <- TIO.readFile file
    print source
    parsed_ast <- eitherParserToIO  $ parse file source
    print parsed_ast
    putStrLn (blue "Parsing completed!")
    putStrLn (blue "Checking for duplicate global variable declerations!")

    eitherStrIO $ checkDuplicateVarDecl parsed_ast
    putStrLn $ blue "No duplicate global variable declerations!"
    (sub, varenv) <- eitherStrIO $ checkGlobalVars parsed_ast
    print varenv
    putStrLn $ blue "Checked Global vars!"
    let parsed_ast2 = mergeTypesGlobalvars parsed_ast varenv
    print parsed_ast2
    result@(sub', env, ast') <- eitherStrIO $ checkFunctions (emptyFunEnv, varenv) parsed_ast2
    print result
    print "Result of fun decl check"
    -- print result



eitherToIO :: Show e => Either e a -> IO a
eitherToIO (Right value) = return value
eitherToIO (Left err) = do
    putStrLn (red "Error: " ++ show err)
    fail $ red "Compilation failed"

-- Same as above but doesn't call show
eitherStrIO :: Either String a -> IO a
eitherStrIO (Right value) = return value
eitherStrIO(Left err) = do
    putStrLn (red "Str Error: " ++ err)
    fail $ red "Compilation failed"