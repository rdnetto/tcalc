{-# LANGUAGE StrictData #-}

module Parser(exprParser) where

import BasicPrelude

import Parser.Common
import Parser.Literals

-- Based on this tutorial: https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html


{- GRAMMAR:

# ISO-8601 is 2018-08-12T22:50:25+00:00, 2018-08-12T22:50:25Z
# TODO: figure out if we want to include TIMESTAMPS


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

exprParser :: Parser expr
exprParser = undefined

