module Main where

import BasicPrelude
import Control.Monad.State.Strict (StateT(..), runStateT)
import Control.Monad.Trans.Except (ExceptT, runExceptT, withExceptT)
import qualified Data.Text as T
import System.Console.ANSI (SGR(SetColor, Reset), ConsoleLayer(Foreground), ColorIntensity(Vivid), Color(Red), setSGR)
import System.Console.Haskeline (MonadException(..), RunIO(..), InputT, runInputT, defaultSettings, getInputLine)
import System.Exit (exitFailure)
import Text.Megaparsec (SourcePos(..))
import Text.Megaparsec.Pos (unPos)

import ArgParser
import Interpreter
import InterpreterT
import Parser.Common
import Parser.Pos
import Parser.Statement
import Types


main :: IO ()
main = do
    args <- parseArgs
    case args of
         Just fp -> runInterpreterT $ runScript fp
         Nothing -> runInterpreterT $ repl "> " evalLine

-- Runs the specified script
runScript :: FilePath -> InterpreterT IO ()
runScript fp = handleErrors True body where
    body = mapM_ execStatement
            =<< runParser' programParser
            =<< readFile fp

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

-- Body of the REPL loop.
-- Note that we discard the failure of each line, which would be incorrect for a script (TODO)
evalLine :: (MonadIO m, MonadInterpreter m)
         => Text -> m ()
evalLine txt = handleErrors False body where
    body = execStatement =<< runParser' (withPos statementParser) txt


-- Orphan instances, needed since the library doesn't depend on haskeline
instance MonadException m => MonadException (InterpreterT m) where
    controlIO f = InterpreterT . StateT $ \s -> controlIO $ \(RunIO run) -> let
                    runInt = flip runStateT s . unInterpreterT
                    run' = RunIO (fmap (InterpreterT . StateT . const) . run . runInt)
                    in fmap runInt $ f run'

instance MonadInterpreter m => MonadInterpreter (InputT m) where
    liftState = lift . liftState


-- runExceptT with error handling
-- if terminal is true, we terminate on errors
handleErrors :: MonadIO m
             => Bool -> ExceptT Text m () -> m ()
handleErrors terminal action = f =<< runExceptT action where
    f (Right ())  = pure ()
    f (Left err) | terminal  = printError err >> liftIO exitFailure
                 | otherwise = printError err

-- Runs a statement, including the position info in any errors thrown
execStatement :: (MonadInterpreter m, MonadIO m)
              => ParseResult Statement -> ExceptT Text m ()
execStatement (ParseResult stmt pos) = withExceptT (renderPos pos) (runStatement stmt)

-- Used to display errors
renderPos :: SourcePos -> Text -> Text
renderPos (SourcePos name line col) msg = res where
    res = concat [
            T.pack name,
            ":",
            tshow $ unPos line,
            ":",
            tshow $ unPos col,
            ":\n",
            msg
        ]

printError :: MonadIO m => Text -> m ()
printError err = liftIO $ do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn $ "ERROR " ++ err
    setSGR [Reset]

