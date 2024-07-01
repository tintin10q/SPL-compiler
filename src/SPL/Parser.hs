{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module SPL.Parser (parse, formatError, eitherParserToIO) where

import SPL.AST (Phase(ParsedP), Program)
import SPL.Parser.Program (pProgram)

import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Void (Void)
import qualified Data.Text as T
import qualified Text.Megaparsec as M

parse :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) (Program ParsedP)
parse = M.parse pProgram

formatError :: ParseErrorBundle T.Text Void -> String
formatError = M.errorBundlePretty


eitherParserToIO :: Either (ParseErrorBundle T.Text Void) a -> IO a
eitherParserToIO (Right value) = return value
eitherParserToIO (Left err) = do
    fail $ formatError err
    -- fail $ red ("Compilation failed") 

