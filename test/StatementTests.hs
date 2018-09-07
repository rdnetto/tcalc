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
            testProgramParse "single line" [
                    "print 1"
                ] [
                    PrintStatement (ExprLiteral (LitScalar 1.0))
                ],
            testProgramParse "multiple lines" [
                    "print 1",
                    "print 2"
                ] []
        ]
    ]

testProgramParse :: TestName ->  [Text] -> [Statement] -> TestTree
testProgramParse name lines expected = testCase name $ do
    case runParser' programParser (unlines lines) of
        Right res -> assertEqual "" expected $ map parseValue res
        Left  err -> assertFailure (textToString err)


