module Main where

import BasicPrelude
import qualified Data.Text as T
import System.Console.Haskeline (MonadException, InputT, runInputT, defaultSettings, getInputLine)

import Interpreter
import Parser.Common (runParser')
import Parser.Statement


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
    process Nothing       = return ()
    process (Just "quit") = return ()
    process (Just "exit") = return ()
    process (Just input) = f (T.pack input) >> loop


-- TODO: add variables, support for writing scripts
evalLine :: MonadIO m => Text -> m ()
evalLine txt = do
    case runParser' statementParser txt of
         Right ast -> runStatement ast
         Left  err -> printError (T.pack err)
