import Test.HUnit

import AdditionCombinationFinder


tests = test
    [ "finds no combinations for 1" ~: do
        [[]] @=? findAdditionCombinations 1
    ]


main :: IO Counts
main = runTestTT tests
