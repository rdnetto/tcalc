module Main where

import BasicPrelude hiding (getContents)
import qualified Prelude as P
import System.IO.Error (ioError, catchIOError, isEOFError)

import Interpreter
import Parser.Common (runParser')
import Parser.Expression
import Parser.Literals


main :: IO ()
main = ignoreEOF . forever $ do
    putStr "> "
    evalLine =<< getLine

-- Ignore an EOFError and continue executing.
-- This allows us to treat the exception as a break from the loop.
ignoreEOF :: IO () -> IO ()
ignoreEOF f = catchIOError f handler where
    handler e | isEOFError e = pure ()
              | otherwise    = ioError e

evalLine :: Text -> IO ()
evalLine txt = do
    case runParser' exprParser txt of
         Right ast -> evalAST ast
         Left  err -> P.putStrLn err

evalAST :: Expr -> IO ()
evalAST expr = res where
    res = case evaluateExpr expr of
               Right e  -> putStrLn $ renderLiteral e
               Left msg -> putStrLn msg
