module LiteralsTests (literalsTests) where

import BasicPrelude
import Test.Tasty
import Test.Tasty.HUnit

import Parser.Literals


literalsTests :: TestTree
literalsTests = testGroup "LiteralsTests" [
        testGroup "renderLiterals" [
            testDurationRendering 93784 "1d2h3m4s",
            testDurationRendering 1.5   "1.5s",
            testDurationRendering 1     "1s",
            testDurationRendering 0.5   "0.5s",
            testDurationRendering 0     "0s"
        ]
    ]

testDurationRendering :: Double -> Text -> TestTree
testDurationRendering secs str = testCase (textToString name) (assertEqual "" str actual) where
    name = "testDurationRendering " ++ str
    actual = renderLiteral (LitDuration $ Duration secs)
