module Parser.Identifier (Identifier(..), idParser) where

import BasicPrelude
import qualified Data.Text as T
import Text.Megaparsec (many)
import Text.Megaparsec.Char (letterChar, alphaNumChar, char)

import Parser.Common
import Types


-- Reserved keywords which may not be used as identifiers
reserved :: [Text]
reserved = ["let", "print", "now"]

idParser ::  Parser Identifier
idParser = do
    let idP = (:)
            <$> letterChar
            <*> many (alphaNumChar <|> char '_')
    name <- T.pack <$> lexeme idP

    when (name `elem` reserved)
        $ fail (T.unpack name ++ " is a reserved keyword and cannot be used as an identifer")

    return $ Identifier name

