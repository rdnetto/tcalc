module Parser.Statement(statementParser, programParser) where

import BasicPrelude
import Control.Monad.Combinators (sepEndBy, many)
import Text.Megaparsec (try, optional)
import Text.Megaparsec.Char (eol)

import Parser.Common
import Parser.Expression
import Parser.Identifier
import Parser.Pos
import Types


statementParser :: Parser Statement
statementParser = try printP <|> letP where
    lSymbol = lexeme . symbol

    printP = optional (lSymbol "print")
            *>  (PrintStatement <$> exprParser)

    letP = lSymbol "let"
            *>  pure LetStatement
            <*> idParser
            <*  lSymbol "="
            <*> exprParser

programParser :: Parser [ParseResult Statement]
programParser = (withPos statementParser `sepEndBy` eol) <* many eol where
