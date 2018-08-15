{-# LANGUAGE StrictData #-}

module Parser(exprParser) where

import BasicPrelude
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void Text

{- GRAMMAR:

# ISO-8601 is 2018-08-12T22:50:25+00:00, 2018-08-12T22:50:25Z
# TODO: figure out if we want to include TIMESTAMPS

SCALAR = any decimal repr, including negatives

DURATION = [0w]?[0d]?[0h]?[0m]?[0s]       # each 0 here is actually our scalar defn, not just an int
         | [00:]00:00[.000]               # 00:00 should parse as mm:ss
                                          # note that the colon must be present to avoid overlapping with scalars

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

data Literal
    = Scalar Float
    | LitDuration Duration
    deriving (Eq, Show)

-- This is stored as the number of seconds.
newtype Duration = Duration Float
    deriving (Eq, Show)

exprParser :: Parser expr
exprParser = undefined
