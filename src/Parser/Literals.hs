{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Parser.Literals (Literal(..), Duration(..), literalParser, renderLiteral, isScalar, isDuration) where

import BasicPrelude
import Control.Applicative (some)
import Data.Text (singleton)
import Prelude (Semigroup)
import Text.Megaparsec (try)
import Text.Megaparsec.Char (oneOf)
import qualified Text.Megaparsec.Char.Lexer as L

import Parser.Common


data Literal
    = LitScalar Double
    | LitDuration Duration
    deriving (Eq, Show)

-- This is stored as the number of seconds.
newtype Duration = Duration Double
    deriving (Eq, Show)

isScalar :: Literal -> Bool
isScalar (LitScalar _) = True
isScalar _ = False

isDuration :: Literal -> Bool
isDuration (LitDuration _) = True
isDuration _ = False

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


{-
    SCALAR = any decimal repr, including negatives

    DURATION = [0w]?[0d]?[0h]?[0m]?[0s]?       # each 0 here is actually our scalar defn, not just an int
             | [00:]00:00[.000]               # 00:00 should parse as mm:ss
-}

-- Association list mapping units to multipliers
-- define each unit in terms of of the preceding one
relativeUnits :: [(Char, Int)]
relativeUnits = [
        ('s', 1),
        ('m', 60),
        ('h', 60),
        ('d', 24),
        ('w', 7)
    ]

-- convert them all into seconds
units :: [Char]
units = map fst relativeUnits

absoluteUnits :: [(Char, Int)]
absoluteUnits = zip units
                    (scanl1 (*) (map snd relativeUnits))


-- Parser accepting an unsigned number expressed as a float or integer
scalar :: Parser Double
scalar = try L.float
      <|> map fromIntegral (L.decimal :: Parser Int)

-- Parser accepting a signed number expressed as a float or integer
signedScalar :: Parser Double
signedScalar = L.signed spaceConsumer scalar

literalParser :: Parser Literal
literalParser = try (lexeme durationP) <|> lexeme scalarP where
    scalarP = LitScalar <$> signedScalar

    durationP = map LitDuration
              . L.signed spaceConsumer
              . map concat
              $ some durationToken

-- Parses a single [SCALAR][SUFFIX] segment
durationToken :: Parser Duration
durationToken = do
    val <- scalar
    unit <- oneOf units
    -- This is safe, because the previous line guarrantees our unit is a key in the assoc list
    let Just mul = lookup unit absoluteUnits
    return $ Duration (val * fromIntegral mul)


-- A textual representation which matches the parse format
renderLiteral :: Literal -> Text
renderLiteral (LitScalar s) = tshow s
renderLiteral (LitDuration (Duration totalSecs)) = f (reverse absoluteUnits) totalSecs "" where
    f :: [(Char, Int)]  -- assoc list of units to process
      -> Double         -- remaining no. of secs
      -> Text           -- accumulator of text to append to
      -> Text           -- result

    f [] _ _ = error "This should never happen"
    -- If negative, need to handle sign upfront to ensure floor does the right thing
    f us s "" | s < 0 = f us (-s) ("-")

    -- When there's just seconds left, render the entire thing (omitting decimal place if not relevant)
    -- (but only if there's a non-zero amount *and* the existing string is non-empty)
    f [_] s "" = renderNum s ++ "s"
    f [_] 0 x  = x
    f [_] s x  = x ++ renderNum s ++ "s"

    -- Recursive case
    f ((unit, multiplier):us) s0 res = f us s' res' where
        m = fromIntegral multiplier
        n :: Int
          = floor $ (s0 / m)

        -- Update accumulators
        s' = s0 - (fromIntegral n) * m
        res' = case n of
                    0 -> res
                    _ -> res ++ tshow n ++ singleton unit

renderNum :: Double -> Text
renderNum s = s' where
    sInt :: Int
        = floor s
    s' = if s == fromIntegral sInt
            then tshow sInt
            else tshow s
