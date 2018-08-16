{-# LANGUAGE StrictData #-}

module Parser.Common(Parser, lexeme) where

import BasicPrelude
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text

-- Defines how whitespace + comments are used
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment where
    lineComment = L.skipLineComment "#"
    blockComment = mempty    -- not supported

-- A lexeme is a lexical unit; a token. This combinator handles any whitespace or comments that follow it
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

