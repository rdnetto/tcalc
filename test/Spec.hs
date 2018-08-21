import BasicPrelude
import Test.Tasty

import LiteralsTests


main :: IO ()
main = defaultMain $ testGroup "Tests" [
        literalsTests
    ]

