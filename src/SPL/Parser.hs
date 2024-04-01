{-# LANGUAGE DataKinds #-}
module SPL.Parser (parse, formatError) where

import SPL.Parser.AST (Phase(ParsedP), Program)
import SPL.Parser.Program (pProgram)

import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Void (Void)
import qualified Data.Text as T
import qualified Text.Megaparsec as M

parse :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) (Program ParsedP)
parse = M.parse pProgram

formatError :: ParseErrorBundle T.Text Void -> String
formatError = M.errorBundlePretty
