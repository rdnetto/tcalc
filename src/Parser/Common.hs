module Parser.Common(Parser, lexeme, symbol, parens, spaceConsumer, runParser') where

import BasicPrelude
import Text.Megaparsec (between, runParser, eof)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (parseErrorPretty)

import Types

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


-- Helper function for running a parser.
-- Returns either an error messsage, or the parsed value
runParser' :: Parser a -> Text -> Either String a
runParser' p txt = do
    let res = runParser (p <* eof) "<stdin>" txt
    case res of
         Right x   -> pure x
         Left  err -> Left $ parseErrorPretty err

