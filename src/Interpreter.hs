module Interpreter (printError, printErrorSimple, runStatement) where

import BasicPrelude
import qualified Data.Text as T
import System.Console.ANSI (SGR(SetColor, Reset), ConsoleLayer(Foreground), ColorIntensity(Vivid), Color(Red), setSGR)
import Text.Megaparsec (SourcePos(..))
import Text.Megaparsec.Pos (unPos)

import Parser.Expression
import Parser.Literals
import Parser.Statement

type EvalRes = Either Text Literal


-- Used to display errors
printError :: MonadIO m => SourcePos -> Text -> m ()
printError (SourcePos name line col) err = printErrorSimple msg where
    msg = concat [
            T.pack name,
            ":",
            tshow $ unPos line,
            ":",
            tshow $ unPos col,
            ":\n",
            err
        ]

printErrorSimple :: MonadIO m => Text -> m ()
printErrorSimple err = liftIO $ do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn $ "ERROR " ++ err
    setSGR [Reset]

-- The entrry point for the interpreter
-- TODO: this should run in its own state monad
runStatement :: MonadIO m => Statement -> m ()
runStatement (PrintStatement pos expr) = res where
    res = case evaluateExpr expr of
               Right e  -> print pos >> putStrLn (renderLiteral e)
               Left msg -> printError pos msg
runStatement (LetStatement pos id' expr) = printError pos $ "not implemented" -- TODO


-- Evalutes an expression to its simplest form, or an error message
evaluateExpr :: Expr -> EvalRes
evaluateExpr (ExprLiteral lit) = pure lit
evaluateExpr (BinaryOp op e1 e2) = do
    l1 <- evaluateExpr e1
    l2 <- evaluateExpr e2
    evalBinOp op l1 l2

evalBinOp :: BinaryOperator -> Literal -> Literal -> EvalRes
evalBinOp op l1 l2
    -- All operations are defined for scalars
    | isScalar l1   && isScalar l2                                       = scalarRes
    -- Durations only support plus/minus within their type
    | isDuration l1 && isDuration l2 && op `elem` [BOAdd, BOMinus]       = durationRes
    -- Division of two durations yields a ratio
    | isDuration l1 && isDuration l2 && op == BODivide                   = scalarRes
    -- Multiplication and division are supported for certain combinations
    | isScalar l1   && isDuration l2 && op `elem` [BOMultiply, BODivide] = durationRes
    | isDuration l1 && isScalar l2   && op == BOMultiply                 = durationRes
    | otherwise                                                          = mismatch
    where
        scalarRes   = pure . LitScalar              $ exec op l1 l2
        durationRes = pure . LitDuration . Duration $ exec op l1 l2
        mismatch    = Left $ concat [
                "Invalid operation (type mismatch): ",
                tshow l1,
                T.pack (' ' : boSymbol op : " "),
                tshow l2
            ]

-- Executes an operator, regardless of whether it makes sense to do so
exec :: BinaryOperator -> Literal -> Literal -> Double
exec BOAdd l1 l2      = (litVal l1) + (litVal l2)
exec BOMinus l1 l2    = (litVal l1) - (litVal l2)
exec BOMultiply l1 l2 = (litVal l1) * (litVal l2)
exec BODivide l1 l2   = (litVal l1) / (litVal l2)

-- Extract the raw value of a literal
litVal :: Literal -> Double
litVal (LitScalar s) = s
litVal (LitDuration (Duration d)) = d

