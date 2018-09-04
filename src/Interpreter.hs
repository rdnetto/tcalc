module Interpreter (runStatement) where

import BasicPrelude
import Control.Monad.Except (MonadError(..))
import qualified Data.Text as T

import InterpreterT
import Parser.Expression
import Parser.Literals
import Parser.Statement


-- The entry point for the interpreter
runStatement :: (MonadIO m, MonadInterpreter m, MonadError Text m)
             => Statement -> m ()
runStatement (PrintStatement expr) = do
    e <- evaluateExpr expr
    putStrLn $ renderLiteral e
runStatement (LetStatement varId expr) = setVar varId =<< evaluateExpr expr


-- Evalutes an expression to its simplest form, or an error message
evaluateExpr :: (MonadInterpreter m, MonadError Text m)
             => Expr -> m Literal
evaluateExpr (ExprLiteral lit) = pure lit
evaluateExpr (BinaryOp op e1 e2) = do
    l1 <- evaluateExpr e1
    l2 <- evaluateExpr e2
    evalBinOp op l1 l2

evalBinOp :: MonadError Text m
          => BinaryOperator -> Literal -> Literal -> m Literal
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
        mismatch    = throwError $ concat [
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

