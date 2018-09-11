module Parser.Common(Parser, lexeme, symbol, parens, spaceConsumer, runParser') where

import BasicPrelude
import Control.Monad.Except (MonadError, liftEither)
import Data.Char (isSpace)
import qualified Data.Text as T
import Text.Megaparsec (between, runParser, eof, takeWhile1P)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (parseErrorPretty)

import Types
import Util

-- Defines how whitespace + comments are used
-- Note that this must *not* accept newlines, as we do not allow expressions to be split over multiple lines
spaceConsumer :: Parser ()
spaceConsumer = L.space spaceP lineComment blockComment where
    lineComment = L.skipLineComment "#"
    blockComment = fail "blockComment not supported"
    spaceP = void $ takeWhile1P (Just "white space") (isSpace `and` isNotEOL)
    isNotEOL '\r' = False
    isNotEOL '\n' = False
    isNotEOL _    = True
    and p1 p2 x = p1 x && p2 x

-- A lexeme is a lexical unit; a token. This combinator handles any whitespace or comments that follow it
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


-- Helper function for running a parser.
runParser' :: MonadError Text m
           => Parser a -> Text -> m a
runParser' p txt = liftEither
                 . mapLeft (T.pack . parseErrorPretty)
                 $ runParser (p <* eof) "<stdin>" txt

