module Main where

import System.Environment (getArgs)
import Text.Megaparsec (parse, errorBundlePretty)
import Parser.Program (pProgram)
import qualified Data.Text.IO as TIO
import Test.Test (runTests)

main :: IO ()
main = do
    runTests
    -- file:_ <- getArgs
    -- source <- TIO.readFile file
    -- case parse pProgram file source of
        -- Left err -> putStr (errorBundlePretty err)
        -- Right ast -> print ast
