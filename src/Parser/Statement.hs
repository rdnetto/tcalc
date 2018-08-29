{-# LANGUAGE StrictData #-}

module Parser.Statement(Statement(..), statementParser) where

import BasicPrelude
import Text.Megaparsec (try, optional)

import Parser.Common
import Parser.Expression
import Parser.Identifier


data Statement
    = PrintStatement Expr
    | LetStatement Identifier Expr
    deriving (Eq, Show)

statementParser :: Parser Statement
statementParser = try printP <|> letP where
    lSymbol = lexeme . symbol

    printP = optional (lSymbol "print")
            *>  map PrintStatement exprParser

    letP = lSymbol "let"
            *>  pure LetStatement
            <*> idParser
            <*  lSymbol "="
            <*> exprParser

    {-
    letP = do
        lSymbol "let"
        id' <- idP
        lSymbol "="
        val <- expressionParser
        return $ LetStatement id' val

        -}
