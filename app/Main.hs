{-# LANGUAGE OverloadedStrings #-}
module Main where

import SPL.Parser
-- import SPL.Printer
import SPL.Colors
import System.Environment (getArgs)

import qualified Data.Text.IO as TIO
import SPL.Typechecker2
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
    return_checked_ast <- eitherStrIO $ checkReturns parsed_ast
    putStrLn (blue "Succesfull return path analysis!")
    _ <- eitherStrIO $ checkDuplicateDecls return_checked_ast
    putStrLn $ blue "No duplicate global variable declerations!"
    -- todo, do we need to do anything with this sub?
    (checked_globals_ast, varenv) <- eitherStrIO $ checkGlobalVars return_checked_ast
    putStrLn $ blue "Checked Global vars!"
    putStrLn $ "Global Var env: " ++ show varenv
    (_sub', _env, typceched_ast) <- eitherStrIO $ checkFunctions (defaultFunEnv, varenv) checked_globals_ast
    putStrLn "Typechecked AST:"
    print typceched_ast
    putStr $ blue "Optimizing ast"
    let optimized_ast = collapseBlocks $ map opti typceched_ast
    -- print optimized_ast
    putStrLn $ blue " pruned " ++ green (show (opti_improvement typceched_ast optimized_ast) ++ "%") ++ blue " of tree."
    putStrLn $ blue "Generating ssm ast"

    {-
todo:
    - Fix print functions 
    - Print is build in 
    
    -}



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