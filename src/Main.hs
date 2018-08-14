module Main where

import BasicPrelude
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (parseErrorPretty)

import Parser


main :: IO ()
main = mapM evalLine . map lines $ getContents

evalLine :: LText -> IO ()
evalLine txt = do
    res <- runParser "<stdin>" exprParser txt
    case res of
         Right ast -> evalAST ast
         Left  err -> P.putStrLn $ parseErrorPretty err
