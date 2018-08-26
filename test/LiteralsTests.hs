module LiteralsTests (literalsTests) where

import BasicPrelude
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (runParser)
import Text.Megaparsec (eof)
import Text.Megaparsec.Error (parseErrorPretty)

import Parser.Literals


literalsTests :: TestTree
literalsTests = testGroup "LiteralsTests" [
        testGroup "renderLiterals" [
            testDurationRendering 93784 "1d2h3m4s",
            testDurationRendering 1.5   "1.5s",
            testDurationRendering 1     "1s",
            testDurationRendering 0.5   "0.5s",
            testDurationRendering 0     "0s",
            testDurationRendering (-70) "-1m10s"
        ],
        testGroup "literalParser" [
            testLiteralParser "1.0"      (LitScalar 1),
            testLiteralParser "1"        (LitScalar 1),
            testLiteralParser "-12.34"   (LitScalar (-12.34)),
            testLiteralParser "1.5s"     (LitDuration $ Duration 1.5),
            testLiteralParser "1m"       (LitDuration $ Duration 60),
            testLiteralParser "1d2h3m4s" (LitDuration $ Duration 93784),
            testLiteralParser "0.5s"     (LitDuration $ Duration 0.5),
            testLiteralParser "0s"       (LitDuration $ Duration 0),
            testLiteralParser "-1m10s"   (LitDuration $ Duration (-70))
        ]
    ]

testDurationRendering :: Double -> Text -> TestTree
testDurationRendering secs str = testCase name (assertEqual "" str actual) where
    name = "testDurationRendering " ++ textToString str
    actual = renderLiteral (LitDuration $ Duration secs)

testLiteralParser :: Text -> Literal -> TestTree
testLiteralParser str expected = testCase name res where
    name = "testLiteralParser " ++ textToString str
    res = case runParser (literalParser <* eof) "<stdin>" str of
             Right ast -> assertEqual "" expected ast
             Left  err -> assertFailure $ parseErrorPretty err

