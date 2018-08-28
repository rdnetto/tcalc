module Main where

import BasicPrelude hiding (getContents)
import qualified Data.Text as T
import System.Console.ANSI (SGR(SetColor, Reset), ConsoleLayer(Foreground), ColorIntensity(Vivid), Color(Red), setSGR)
import System.Console.Haskeline (MonadException, InputT, runInputT, defaultSettings, getInputLine)

import Interpreter
import Parser.Common (runParser')
import Parser.Expression
import Parser.Literals


main :: IO ()
main = repl "> " evalLine

-- Helper function for defining a REPL
repl :: forall m
     .  MonadException m
     => String                  -- the prompt to use
     -> (Text -> InputT m ())   -- eval function
     -> m ()
repl prompt f = runInputT defaultSettings loop where
    loop = getInputLine prompt >>= process
    process Nothing      = return ()
    process (Just input) = f (T.pack input) >> loop


evalLine :: MonadIO m => Text -> m ()
evalLine txt = do
    case runParser' exprParser txt of
         Right ast -> evalAST ast
         Left  err -> printError (T.pack err)

evalAST :: MonadIO m => Expr -> m ()
evalAST expr = res where
    res = case evaluateExpr expr of
               Right e  -> putStrLn $ renderLiteral e
               Left msg -> printError msg

-- Used to display errors
printError :: MonadIO m => Text -> m ()
printError err = liftIO $ do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn $ "ERROR: " ++ err
    setSGR [Reset]
