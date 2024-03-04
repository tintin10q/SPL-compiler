module Main where

import SPL.Parser
-- import SPL.Printer

import System.Environment (getArgs)

import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    file:_ <- getArgs
    source <- TIO.readFile file
    case parse file source of
        Left err -> putStr (formatError err)
        Right ast -> do -- putStr $ formatProgram ast 
                        print ast 
