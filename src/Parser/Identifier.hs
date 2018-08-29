module Parser.Identifier (Identifier(..), idParser) where

import BasicPrelude
import qualified Data.Text as T
import Text.Megaparsec (many)
import Text.Megaparsec.Char (letterChar, alphaNumChar, char)

import Parser.Common
import Types

idParser ::  Parser Identifier
idParser = Identifier . T.pack <$> lexeme idP where
    idP = (:)
        <$> letterChar
        <*> many (alphaNumChar <|> char '_')

