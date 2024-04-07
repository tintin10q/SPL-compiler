module Main where

import SPL.Parser
-- import SPL.Printer

import System.Environment (getArgs)

import qualified Data.Text.IO as TIO
import SPL.Parser.AST (empty)

main :: IO ()
main = do
    file:_ <- getArgs
    source <- TIO.readFile file
    case parse file source of
        Left err -> putStr (formatError err)
        Right ast -> print ast 
