import Test.HUnit

import AdditionCombinationFinder


tests = test
    [ "finds no combinations for 1" ~: do
        [[]] @=? findAdditionCombinations 1
    , "finds combination for 2" ~: do
        [[1, 1]] @=? findAdditionCombinations 2
    ]


main :: IO Counts
main = runTestTT tests
