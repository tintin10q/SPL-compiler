module Main where

import Test.Parser.ParserSpec (parserSpec)

import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    parserSpec