{-# LANGUAGE StrictData #-}

module Parser.Identifier (Identifier(..), idParser) where

import BasicPrelude
import qualified Data.Text as T
import Text.Megaparsec (many)
import Text.Megaparsec.Char (letterChar, alphaNumChar, char)

import Parser.Common

-- Identifier of a variable
newtype Identifier = Identifier Text
    deriving (Eq, Show)

idParser ::  Parser Identifier
idParser = Identifier . T.pack <$> lexeme idP where
    idP = (:)
        <$> letterChar
        <*> many (alphaNumChar <|> char '_')

