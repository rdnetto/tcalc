module Parser.Expression(Expr(..), BinaryOperator(..), exprParser, boSymbol) where

import BasicPrelude
import Text.Megaparsec.Expr (makeExprParser, Operator(..))

import Parser.Common
import Parser.Literals
import Types

-- Based on this tutorial: https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

boSymbol :: BinaryOperator -> Char
boSymbol BOAdd      = '+'
boSymbol BOMinus    = '-'
boSymbol BOMultiply = '*'
boSymbol BODivide   = '/'

exprParser :: Parser Expr
exprParser = makeExprParser term ops where
    -- a term in the expression
    term = parens exprParser <|> (ExprLiteral <$> literalParser)

    -- Operator table
    ops = [
            [
                binary "*" BOMultiply,
                binary "/" BODivide
            ], [
                binary "+" BOAdd,
                binary "-" BOMinus
            ]
        ]
    binary name op = InfixL $ BinaryOp op <$ symbol name
