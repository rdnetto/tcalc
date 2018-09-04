module Parser.Common(Parser, ParseResult(..), lexeme, symbol, parens, spaceConsumer, runParser') where

import BasicPrelude
import Control.Monad.Except (MonadError, liftEither)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Text.Megaparsec (between, runParser, eof, getParserState, State(statePos))
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (parseErrorPretty)
import Text.Megaparsec.Pos (SourcePos)

import Types
import Util

-- Defines how whitespace + comments are used
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment where
    lineComment = L.skipLineComment "#"
    blockComment = fail "blockComment not supported"

-- A lexeme is a lexical unit; a token. This combinator handles any whitespace or comments that follow it
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


data ParseResult a = ParseResult {
    parseValue :: a,
    parsePos :: SourcePos
} deriving (Eq, Show)

-- Helper function for running a parser.
-- Returns either an error messsage, or the parsed value
runParser' :: MonadError Text m
           => Parser a -> Text -> m (ParseResult a)
runParser' p txt = liftEither
                 . mapLeft (T.pack . parseErrorPretty)
                 $ runParser p' "<stdin>" txt
    where
        p' = do
            pos <- posP
            res <- p
            eof
            return $ ParseResult res pos

-- Helper function for yielding the current location in the input
posP :: Parser SourcePos
posP = NE.head . statePos <$> getParserState
