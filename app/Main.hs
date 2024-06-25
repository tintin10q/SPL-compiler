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

import SPL.Preprocess
import GHC.Base (when)
import SPL.PrettyPrint
import SPL.Codegen.GenSSM (getSmmCode)
import SPL.Codegen.SSM (formatCode)
import System.Exit (exitSuccess, exitFailure)
import SPL.ArgParse

main :: IO ()
main = do
    args <- getArgs
    when ("--help" `elem` args || "-h" `elem` args || "-help" `elem` args) (putStrLn helpText >> exitSuccess)
    when (null args) $ putStrLn "No input file specified!" >> exitFailure
    let filename = head args
        parsedargs = parseArgs $ tail args
        showInfo = not (hideInfo parsedargs)
        optimize = not (skipOptimizer parsedargs)
        warn = not (hideWarnings parsedargs)
    when showInfo $ putStrLn $ "Reading file " ++ blue filename
    source <- TIO.readFile filename
    -- putStrLn $ blue "Read in Source:\n" ++ T.unpack source
    parsed_ast <- eitherParserToIO  $ parse filename source
    when showInfo $ putStrLn (blue "Parsed AST! ")
    when showInfo $ print parsed_ast
    when showInfo $ putStr (blue "Parsing completed! ")
    eitherStrIO $ checkEmptyBody parsed_ast >> checkHasMain parsed_ast 
    when showInfo $ putStrLn (blue "(program has main function) ")
    -- print parsed_ast
    let preprocessed_ast = preprocesAST parsed_ast
        improvement = opti_improvement parsed_ast preprocessed_ast
    when (improvement < 0 && showInfo) $ putStrLn (blue "Pruned " ++ green (show (-improvement) ++ "%") ++ blue " of tree by removing dead code.")
    when showInfo $ putStrLn $ pretty preprocessed_ast
    return_checked_ast <- eitherStrIO $ checkReturns preprocessed_ast
    -- let return_checked_ast = map upgrade preprocessed_ast
    let explicit_returns_ast = makeVoidReturnsExplicit return_checked_ast
    -- when showInfo $ putStr $ pretty explicit_returns_ast
    when showInfo $ putStrLn (blue "\nSuccesfull return path analysis!")
    _ <- eitherStrIO $ checkDuplicateDecls explicit_returns_ast
    when showInfo $ putStrLn $ blue "No duplicate declerations!"
    -- todo, do we need to do anything with this sub?
    typechecked_ast <- eitherStrIO $ fst $ checkProgram explicit_returns_ast
    when showInfo $ putStrLn $ blue "Typechecked AST:"
    when showInfo $ print typechecked_ast -- Show the types of the expressions 
    when showInfo $ putStrLn $ blue "Pretty Typechecked AST:"
    when showInfo $ putStrLn $ pretty typechecked_ast
  -- Boolean Literal evaluation, todo this makes a small part lazy, which means that if you would have 1 && true it would be ok. So its important that this runs after type checking I think
  -- Also todo we should repeat this opti function until there is no improvement anymore

    let optimized_ast = if optimize then collapseBlocks $ map opti typechecked_ast else typechecked_ast
        improvement' =  if optimize then - opti_improvement typechecked_ast optimized_ast else -0

    -- print optimized_ast
    when (showInfo && optimize) $ putStrLn $ blue "Optimizing step shrunk AST with by " ++ green (show improvement' ++ "%")
    when (improvement' > 0 && showInfo && optimize) (putStrLn $ blue "New Optimised AST:\n" ++  pretty optimized_ast)
    when (not optimize && warn) $ putStrLn (yellow "WARNING:" ++ " Skipped optimalizations because you enabled the "++ blue "--skip-optimizer" ++ " option")

    when showInfo $ putStrLn $ blue "Generating ssm ast"
    let code = getSmmCode parsedargs optimized_ast
        formatted_code = formatCode code
    -- "#!java -jar ssm/ssm.jar\n"
    let outFile = getOutputFile parsedargs
    writeFile outFile formatted_code
    when warn $ putStrLn $ "Wrote " ++ yellow (show (length code)) ++ " instructions to " ++ green outFile


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


-- Get it to run automatically, and also executable that would be cool, stdin or path is already nice, maybe a special option
-- But not really needed you just press r. But cool if it runs in cli. It almost looks like a real program. But wasm though..
{-
usage: [--clisteps <steps>] [--haltonerror] [--cli] [--file <path> OR --stdin]
  --help             : Print this help
  --version          : Print version
  --clisteps <steps> : The amount of steps to run. -1 for infinite(default). Only in cli mode
  --stdin            : Read code from stdin
  --file <path>      : Read code from path
  --cli              : No GUI, runs code and exits on halt
  --haltonerror      : Halt on error. Only in cli mode
  --guidelay         : Amount of time to sleep in milliseconds between steps in the GUI. Default: 50
-}


helpText :: String
helpText = unlines [blue "SPL Compiler" ++ " By Quinten Cabo",
                    "This compiler compiles the SPL programming language into ssm.",
                    "The first argument should be the filename of the .spl file to compile.",
                    "Possible further arguments:",
                    "--output (or -o) <filename> \t default: output.ssm",
                    "--hide-info \t\t Optional argument to hide info messages",
                    "--hide-warnings \t Optional argument to hide warning messages",
                    "--skip-optimizer \t Optional argument to skip the optimizer step",
                    "--help (or -h) \t\t Print this help message"
                    ]