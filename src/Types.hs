{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Types where

import BasicPrelude
import Data.Void (Void)
import Prelude (Semigroup)
import Text.Megaparsec (Parsec)


-- The parser type
type Parser = Parsec Void Text

-- Identifier of a variable
newtype Identifier = Identifier Text
    deriving (Eq, Show, Hashable)


-- Literals
data Literal
    = LitScalar Double
    | LitDuration Duration
    deriving (Eq, Show)

isScalar :: Literal -> Bool
isScalar (LitScalar _) = True
isScalar _ = False

isDuration :: Literal -> Bool
isDuration (LitDuration _) = True
isDuration _ = False


-- This is stored as the number of seconds.
newtype Duration = Duration Double
    deriving (Eq, Show)

-- Multiplication is non-sensical for durations, so the monoid instance is unambiguously addition
instance Semigroup Duration where
    (Duration a) <> (Duration b) = Duration (a + b)

instance Monoid Duration where
    mempty = Duration 0

-- Not technically valid because we don't implement (*), but that's what megaparsec's
-- signed combinator is implemented in terms of, so...
instance Num Duration where
    (Duration a) + (Duration b) = Duration (a + b)
    (Duration a) - (Duration b) = Duration (a - b)
    (Duration _) * (Duration _) = error "Multiplication is not valid for Durations"
    negate (Duration d) = Duration (negate d)
    abs (Duration d) = Duration (abs d)
    signum (Duration d) = Duration (signum d)
    fromInteger = Duration . fromInteger


data Expr
    = BinaryOp BinaryOperator Expr Expr     -- a binary operator
    | ExprLiteral Literal                   -- a literal
    | ExprVarRef Identifier                 -- a variable
    deriving (Eq, Show)

data BinaryOperator
    = BOAdd
    | BOMinus
    | BOMultiply
    | BODivide
    deriving (Eq, Show)


data Statement
    = PrintStatement Expr
    | LetStatement Identifier Expr
    deriving (Eq, Show)

