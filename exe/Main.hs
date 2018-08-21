module Main where

import BasicPrelude hiding (getContents)
import Data.Text.IO (getContents)
import qualified Prelude as P
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (parseErrorPretty)

import Interpreter
import Parser.Expression
import Parser.Literals


main :: IO ()
main = do
    txt <- getContents
    mapM_ evalLine $ lines txt

evalLine :: Text -> IO ()
evalLine txt = do
    let res = runParser exprParser "<stdin>" txt
    case res of
         Right ast -> evalAST ast
         Left  err -> P.putStrLn $ parseErrorPretty err

evalAST :: Expr -> IO ()
evalAST expr = res where
    res = case evaluateExpr expr of
               Right e  -> putStrLn $ renderLiteral e
               Left msg -> putStrLn msg
