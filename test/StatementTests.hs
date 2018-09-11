module StatementTests (statementTests) where

import BasicPrelude hiding (lines)
import Test.Tasty
import Test.Tasty.HUnit

import Parser.Common (runParser')
import Parser.Pos
import Parser.Statement
import Types


statementTests :: TestTree
statementTests = testGroup "StatementTests" [
        testGroup "programParser" [
            testProgramParse "single line (no newline)"
                    "print 1"
                [
                    PrintStatement (ExprLiteral (LitScalar 1.0))
                ],
            testProgramParse "single line (with newline)"
                    "print 1\n"
                [
                    PrintStatement (ExprLiteral (LitScalar 1.0))
                ],
            testProgramParse "single line (extra newlines)"
                    "print 1\n\n"
                [
                    PrintStatement (ExprLiteral (LitScalar 1.0))
                ],
            testProgramParse "multiple lines" (unlines [
                    "print 1",
                    "print 2"
                ])
                [
                    PrintStatement (ExprLiteral (LitScalar 1.0)),
                    PrintStatement (ExprLiteral (LitScalar 2.0))
                ]
        ]
    ]

testProgramParse :: TestName ->  Text -> [Statement] -> TestTree
testProgramParse name input expected = testCase name $ do
    case runParser' programParser input of
        Right res -> assertEqual "" expected $ map parseValue res
        Left  err -> assertFailure (textToString err)


