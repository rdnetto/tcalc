{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Parser.Literals (Literal(..), Duration(..), literalParser, renderLiteral) where

import BasicPrelude
import Control.Applicative (some)
import Data.Text (singleton)
import Prelude (Semigroup)
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


-- Multiplication is non-sensical for durations, so the monoid instance is unambiguously addition
instance Semigroup Duration where
    (Duration a) <> (Duration b) = Duration (a + b)

instance Monoid Duration where
    mempty = Duration 0


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


literalParser :: Parser Literal
literalParser = lexeme (scalarP <|> durationP) where
    scalarP = LitScalar <$> L.float

    -- Parses a single [SCALAR][SUFFIX] segment
    durationToken :: Parser Duration
    durationToken = do
        val <- L.float
        unit <- oneOf units
        -- This is safe, because the previous line guarrantees our unit is a key in the assoc list
        let Just mul = lookup unit absoluteUnits
        return $ Duration (val * fromIntegral mul)

    durationP = map LitDuration
              . map concat
              $ some durationToken


-- A textual representation which matches the parse format
renderLiteral :: Literal -> Text
renderLiteral (LitScalar s) = tshow s
renderLiteral (LitDuration (Duration totalSecs)) = f (reverse absoluteUnits) totalSecs "" where
    f :: [(Char, Int)] -> Double -> Text -> Text
    f [] _ _ = error "This should never happen"
    -- When there's just seconds left, render the entire thing (omitting decimal place if not relevant)
    f [_] s x = x ++ s' ++ "s" where
        sInt :: Int
            = floor s
        s' = if s == fromIntegral sInt
                then tshow sInt
                else tshow s

    f ((unit, multiplier):us) s0 res = f us s' res' where
        m = fromIntegral multiplier
        n :: Int
          = floor $ (s0 / m)

        -- Update accumulators
        s' = s0 - (fromIntegral n) * m
        res' = case n of
                    0 -> res
                    _ -> res ++ tshow n ++ singleton unit
