module PartitionsTest
  ( tests
  ) where

import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

import Test.HUnit

import Partitions

additionCombinationsTests =
  [ "finds no combinations for 0" ~: do
    Set.fromList [] @=? additionCombinations 0
  , "finds combination for 2" ~: do
    Set.fromList [[1, 1], [2]] @=? additionCombinations 2
  , "finds combinations for 7" ~: do
    let
      combinationsOf7 =
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
    Set.fromList combinationsOf7 @=? additionCombinations 7
  ]

expandedTailCombinationTests =
  [ "separates 1 from 2" ~: do
    [1, 1] @=? expandedTailCombination [2]
  , "moves 1 from 3 into tail of [3, 1]" ~: do
    [2, 1, 1] @=? expandedTailCombination [3, 1]
  ]

collapsedTailCombosTests =
  [ "finds 3 tails when given [3, 1, 1, 1, 1]" ~: do
    3 @=? (length . collapsedTailCombos $ [3, 1, 1, 1, 1])
  , "finds [3, 2, 1, 1] when finding tails of [3, 1, 1, 1, 1]" ~: do
    True @=? (elem [3, 2, 1, 1] $ collapsedTailCombos [3, 1, 1, 1, 1])
  , "finds [3, 2, 2] when finding tails of [3, 1, 1, 1, 1]" ~: do
    True @=? (elem [3, 2, 2] $ collapsedTailCombos [3, 1, 1, 1, 1])
  , "finds [3, 3, 1] when finding tails of [3, 1, 1, 1, 1]" ~: do
    True @=? (elem [3, 3, 1] $ collapsedTailCombos [3, 1, 1, 1, 1])
  ]

collapseIntoTests =
  [ "does not collapse when split at less than 1" ~: do
    Nothing @=? collapseInto 0 [1]
  , "does not collapse when right-hand side of split is empty" ~: do
    Nothing @=? collapseInto 1 [1]
  , "does not collapse when right-hand side of split has only one element" ~:
    do Nothing @=? collapseInto 1 [2, 1]
  , "does not collapse when rhs head >= tail of one-element lhs list" ~: do
    Nothing @=? collapseInto 1 [2, 2, 1]
  , "does not collapse when rhs sum > tail of one-element lhs list" ~: do
    Nothing @=? collapseInto 1 [3, 2, 2]
  , "does not collapse when rhs head >= tail of two-element lhs list" ~: do
    Nothing @=? collapseInto 2 [3, 2, 2, 1]
  , "collapses 1 into head of two-element right-hand side list" ~: do
    (Just [3, 3]) @=? collapseInto 1 [3, 2, 1]
  , "collapses 1 into rhs head when both sides have 2 elements" ~: do
    (Just [4, 3, 3]) @=? collapseInto 2 [4, 3, 2, 1]
  , "collapses 1 into head of three-element rhs list" ~: do
    (Just [3, 3, 1]) @=? collapseInto 1 [3, 2, 1, 1]
  ]

tests = expandedTailCombinationTests
     ++ collapsedTailCombosTests
     ++ collapseIntoTests
     ++ additionCombinationsTests
