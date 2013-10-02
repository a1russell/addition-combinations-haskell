import Test.HUnit

import AdditionCombinationFinder


tests = test
    [ "finds no combinations for 0" ~: do
        [] @=? findAdditionCombinations 0
    , "finds combination for 2" ~: do
        [[1, 1], [2]] @=? findAdditionCombinations 2
    ]


main :: IO Counts
main = runTestTT tests
