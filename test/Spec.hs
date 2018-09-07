import BasicPrelude
import Test.Tasty

import LiteralsTests
import StatementTests


main :: IO ()
main = defaultMain $ testGroup "Tests" [
        literalsTests,
        statementTests
    ]

