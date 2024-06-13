{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- todo maybe we can remove endPos and startPos exports
module SPL.Parser.Parser (Parser) where

import Text.Megaparsec (Parsec)
import Data.Text (Text)
import Data.Void (Void)

type Parser = Parsec Void Text
--                   ^    ^
--                   |    |
-- Custom error component Type of input stream
-- Until we start dealing with custom parsing errors, assume this type.


