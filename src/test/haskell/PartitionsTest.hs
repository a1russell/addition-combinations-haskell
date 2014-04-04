module PartitionsTest
  ( tests
  ) where

import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

import Test.HUnit

import Partitions

partitionsTests =
  [ "finds no partitions for 0" ~: do
    Set.fromList [] @=? partitions 0
  , "finds partitions for 2" ~: do
    Set.fromList [[1, 1], [2]] @=? partitions 2
  , "finds partitions for 7" ~: do
    let
      partitionsOf7 =
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
    Set.fromList partitionsOf7 @=? partitions 7
  ]

expandedTailPartitionTests =
  [ "separates 1 from 2" ~: do
    [1, 1] @=? expandedTailPartition [2]
  , "moves 1 from 3 into tail of [3, 1]" ~: do
    [2, 1, 1] @=? expandedTailPartition [3, 1]
  ]

collapsedTailPartitionsTests =
  [ "finds 3 tails when given [3, 1, 1, 1, 1]" ~: do
    3 @=? (length . collapsedTailPartitions $ [3, 1, 1, 1, 1])
  , "finds [3, 2, 1, 1] when finding tails of [3, 1, 1, 1, 1]" ~: do
    True @=? (elem [3, 2, 1, 1] $ collapsedTailPartitions [3, 1, 1, 1, 1])
  , "finds [3, 2, 2] when finding tails of [3, 1, 1, 1, 1]" ~: do
    True @=? (elem [3, 2, 2] $ collapsedTailPartitions [3, 1, 1, 1, 1])
  , "finds [3, 3, 1] when finding tails of [3, 1, 1, 1, 1]" ~: do
    True @=? (elem [3, 3, 1] $ collapsedTailPartitions [3, 1, 1, 1, 1])
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

partitionsCountTests =
  [ "count of partitions of -1 is 0" ~: do
    0 @=? partitionsCount (-1)
  , "count of partitions of 0 is 1" ~: do
    1 @=? partitionsCount 0
  , "count of partitions of 1 is 1" ~: do
    1 @=? partitionsCount 1
  , "count of partitions of 10 is 42" ~: do
    42 @=? partitionsCount 10
  ]

tests = expandedTailPartitionTests
     ++ collapsedTailPartitionsTests
     ++ collapseIntoTests
     ++ partitionsTests
     ++ partitionsCountTests
