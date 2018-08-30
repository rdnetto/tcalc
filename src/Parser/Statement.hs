module Parser.Statement(Statement(..), statementParser) where

import BasicPrelude
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec (try, optional, getParserState, State(statePos))

import Parser.Common
import Parser.Expression
import Parser.Identifier
import Types


statementParser :: Parser Statement
statementParser = try printP <|> letP where
    lSymbol = lexeme . symbol

    printP = optional (lSymbol "print")
            *>  (PrintStatement <$> pos <*> exprParser)

    letP = lSymbol "let"
            *>  pure LetStatement
            <*> pos
            <*> idParser
            <*  lSymbol "="
            <*> exprParser

    pos = NE.head . statePos <$> getParserState
