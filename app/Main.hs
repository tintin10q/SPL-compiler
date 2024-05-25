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
import SPL.Optimizer (opti, collapseBlocks, opti_improvement)
import SPL.Return

import qualified Data.Text as T

main :: IO ()
main = do
    file:_ <- getArgs
    source <- TIO.readFile file
    putStrLn $ blue "Read Source:\n" ++ T.unpack source
    parsed_ast <- eitherParserToIO  $ parse file source
    putStrLn (blue "Parsing completed!")
    print parsed_ast
    _ <- eitherStrIO $ returns parsed_ast
    putStrLn (blue "Succesfull return path analysis!")
    eitherStrIO $ checkDuplicateVarDecl parsed_ast
    putStrLn $ blue "No duplicate global variable declerations!"
    -- todo, do we need to do anything with this sub?
    (sub, env) <- eitherStrIO $ checkGlobalVars parsed_ast
    putStrLn $ blue "Checked Global vars!"
    let (funenv, varenv) = env
    putStrLn $ "Global Var env: " ++ show varenv
    let parsed_ast2 = mergeTypesGlobalvars parsed_ast varenv
    -- print parsed_ast2
    result@(sub', env, ast') <- eitherStrIO $ checkFunctions (defaultFunEnv, varenv) parsed_ast2
    print "Result of fun decl check"
    print result
    putStrLn $ blue "Optimizing ast"
    let optimized_ast = collapseBlocks $ map opti ast'
    print optimized_ast
    putStrLn $ blue "Optimizing ast pruned " ++ green (show (opti_improvement ast' optimized_ast) ++ "%") ++ " of tree."



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