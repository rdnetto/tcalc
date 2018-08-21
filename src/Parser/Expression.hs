{-# LANGUAGE StrictData #-}

module Parser.Expression(Expr(..), BinaryOperator(..), exprParser, boSymbol) where

import BasicPrelude
import Text.Megaparsec.Expr (makeExprParser, Operator(..))

import Parser.Common
import Parser.Literals

-- Based on this tutorial: https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html


{- GRAMMAR:
# TODO: figure out if we want to include TIMESTAMPS
# ISO-8601 is 2018-08-12T22:50:25+00:00, 2018-08-12T22:50:25Z


# All of these yield durations
EXPR = DURATION [+-] DURATION
     | DURATION [*/] SCALAR
     | SCALAR * DURATION
     | (EXPR)
-}

data Expr
    = BinaryOp BinaryOperator Expr Expr
    | ExprLiteral Literal
    deriving (Eq, Show)

data BinaryOperator
    = BOAdd
    | BOMinus
    | BOMultiply
    | BODivide
    deriving (Eq, Show)

boSymbol :: BinaryOperator -> Char
boSymbol BOAdd      = '+'
boSymbol BOMinus    = '-'
boSymbol BOMultiply = '*'
boSymbol BODivide   = '/'

exprParser :: Parser Expr
exprParser = makeExprParser term ops where
    -- a term in the expression
    term = parens exprParser <|> (ExprLiteral <$> literalParser)

    -- Operator table
    ops = [
            [
                binary "*" BOMultiply,
                binary "/" BODivide
            ], [
                binary "+" BOAdd,
                binary "-" BOMinus
            ]
        ]
    binary name op = InfixL $ BinaryOp op <$ symbol name
