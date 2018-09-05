module Parser.Pos(ParseResult(..), withPos) where

import BasicPrelude
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec (SourcePos(..), getParserState, statePos)

import Parser.Common


data ParseResult a = ParseResult {
    parseValue :: a,
    parsePos :: SourcePos
} deriving (Eq, Show)


-- Helper functions for yielding the current location in the input
posP :: Parser SourcePos
posP = NE.head . statePos <$> getParserState

withPos :: Parser a -> Parser (ParseResult a)
withPos p = (flip ParseResult) <$> posP <*> p
