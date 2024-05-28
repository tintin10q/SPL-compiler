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
import SPL.Preprocess
import GHC.Base (when)
import SPL.PrettyPrint
import SPL.Codegen.GenSSM (getSmmCode)
import SPL.Codegen.SSM (formatCode)

-- import System.Posix.Files (getFileStatus, fileMode, setFileMode, ownerExecuteMode, groupExecuteMode, otherExecuteMode)
-- import System.Posix.Types (FileMode)

defaultOutput :: String
defaultOutput = "output.ssm"

-- Function to parse arguments and get the output value
getOutputValue :: [String] -> String
getOutputValue [] = defaultOutput
getOutputValue ("-o":value:_) = value
getOutputValue (_:xs) = getOutputValue xs

-- Add option to disable optimizer

main :: IO ()
main = do
    args <- getArgs
    let outputFile = getOutputValue args
    -- todo better error msg for this
    let file:_ = args 
    source <- TIO.readFile file
    -- putStrLn $ blue "Read in Source:\n" ++ T.unpack source
    parsed_ast <- eitherParserToIO  $ parse file source
    putStr (blue "Parsing completed! ")
    eitherStrIO $ checkHasMain parsed_ast
    putStrLn (blue "(program has main function) ")
    -- print parsed_ast
    let preprocessed_ast = preprocesAST parsed_ast
        improvement = opti_improvement parsed_ast preprocessed_ast
    when (improvement < 0) $ putStrLn (blue "Pruned " ++ green (show (-improvement) ++ "%") ++ blue " of tree by removing dead code.")
    putStrLn $ pretty preprocessed_ast
    return_checked_ast <- eitherStrIO $ checkReturns preprocessed_ast
    putStrLn (blue "Succesfull return path analysis!")
    eitherStrIO $ checkDuplicateDecls return_checked_ast
    putStrLn $ blue "No duplicate declerations!"
    -- todo, do we need to do anything with this sub?
    -- (checked_globals_ast, varenv) <- eitherStrIO $ checkGlobalVars return_checked_ast
    -- putStrLn $ blue "Checked Global vars!"
    -- putStrLn $ "Global Var env:\n" ++ prettyPrintMap varenv
    -- putStrLn $ pretty checked_globals_ast
    (_sub', _env, typceched_ast) <- eitherStrIO $ checkProgram (defaultFunEnv, defaultVarEnv) return_checked_ast
    putStrLn $ blue "Typechecked AST:"
    putStrLn $ pretty typceched_ast
  -- Boolean Literal evaluation, todo this makes a small part lazy, which means that if you would have 1 && true it would be ok. So its important that this runs after type checking I think
  -- Also todo we should repeat this opti function until there is no improvement anymore
    let optimized_ast = collapseBlocks $ map opti typceched_ast
        improvement' = - opti_improvement typceched_ast optimized_ast
    -- print optimized_ast
    putStrLn $ blue "Optimizing step shrunk AST with by " ++ green (show improvement' ++ "%")
    when (improvement' > 0) (putStrLn $ blue "New Optimised AST:" ++  pretty optimized_ast)
    putStrLn $ blue "Generating ssm ast"
    let code = getSmmCode optimized_ast
        formatted_code = formatCode code
    putStrLn formatted_code
    -- "#!java -jar ssm/ssm.jar\n"
    writeFile outputFile formatted_code
    putStrLn $ "Wrote " ++ show (length code) ++ " instructions to " ++ outputFile
    return undefined


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

