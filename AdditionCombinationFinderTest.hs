import qualified Data.Set as Set
import Data.Maybe

import Test.HUnit

import AdditionCombinationFinder

findAdditionCombinationsTests =
    [ "finds no combinations for 0" ~: do
        Set.fromList [] @=? findAdditionCombinations 0
    , "finds combination for 2" ~: do
        Set.fromList [[1, 1], [2]] @=? findAdditionCombinations 2
    , "finds combinations for 7" ~: do
        let combinationsOf7 =
                [ [1, 1, 1, 1, 1, 1, 1]
                , [2, 2, 2, 1]
                , [2, 2, 1, 1, 1]
                , [2, 1, 1, 1, 1, 1]
                , [3, 2, 2]
                , [3, 3, 1]
                , [3, 2, 1, 1]
                , [3, 1, 1, 1, 1]
                , [4, 3]
                , [4, 2, 1]
                , [4, 1, 1, 1]
                , [5, 2]
                , [5, 1, 1]
                , [6, 1]
                , [7]
                ]
        Set.fromList combinationsOf7 @=? findAdditionCombinations 7
    ]

moveOneFromHeadToTailTests =
    [ "separates 1 from 2" ~: do
        [1, 1] @=? moveOneFromHeadToTail [2]
    , "moves 1 from 3 into tail of [3, 1]" ~: do
        [2, 1, 1] @=? moveOneFromHeadToTail [3, 1]
    ]

findTailsTests =
    [ "finds 3 non-Nothing tails when given [3, 1, 1, 1, 1]" ~: do
        3 @=? (length . catMaybes . findTails . Just $ [3, 1, 1, 1, 1])
    , "finds [3, 2, 1, 1] when finding tails of [3, 1, 1, 1, 1]" ~: do
        True @=? (elem [3, 2, 1, 1] $ catMaybes . findTails . Just $ [3, 1, 1, 1, 1])
    , "finds [3, 2, 2] when finding tails of [3, 1, 1, 1, 1]" ~: do
        True @=? (elem [3, 2, 2] $ catMaybes . findTails . Just $ [3, 1, 1, 1, 1])
    , "finds [3, 3, 1] when finding tails of [3, 1, 1, 1, 1]" ~: do
        True @=? (elem [3, 3, 1] $ catMaybes . findTails . Just $ [3, 1, 1, 1, 1])
    ]

absorbTailTests =
    [ "does not absorb tail when left-hand side is empty" ~: do
        Nothing @=? absorbTail [] [1]
    , "does not absorb tail when right-hand side is empty" ~: do
        Nothing @=? absorbTail [1] []
    , "does not absorb tail when right-hand side has only one element" ~: do
        Nothing @=? absorbTail [2] [1]
    , "does not absorb tail when rhs head >= lhs tail of one-element list" ~: do
        Nothing @=? absorbTail [2] [2, 1]
    , "does not absorb tail when rhs sum > lhs tail of one-element list" ~: do
        Nothing @=? absorbTail [2] [1, 2]
    , "does not absorb tail when rhs head >= lhs tail of two-element list" ~: do
        Nothing @=? absorbTail [3, 2] [2, 1]
    , "absorbs 1 into right-hand head of two-element list" ~: do
        (Just [3, 3]) @=? absorbTail [3] [2, 1]
    , "absorbs 1 into rhs head when both sides have 2 elements" ~: do
        (Just [4, 3, 3]) @=? absorbTail [4, 3] [2, 1]
    , "absorbs 1 into head of three-element list" ~: do
        (Just [3, 3, 1]) @=? absorbTail [3] [2, 1, 1]
    ]

tests = test $ moveOneFromHeadToTailTests
             ++ findTailsTests
             ++ absorbTailTests
             ++ findAdditionCombinationsTests

main :: IO Counts
main = runTestTT tests
