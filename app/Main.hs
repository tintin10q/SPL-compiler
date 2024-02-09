module Main where

import Parser.AST
import Parser.Parser
import Test.Test (runTests)

main :: IO ()
--main = putStrLn "Hello, Haskell!" 
main = do runTests 

