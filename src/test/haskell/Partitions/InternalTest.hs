module Partitions.InternalTest
  ( tests
  ) where

import Control.Monad.Reader (runReader)
import Data.Set (fromList)

import Test.HUnit

import Partitions.Internal

partitions'' :: Int -> Partitions
partitions'' args =
  let
    env = PartitionsEnv appendOneAndMaybeIncrementLast
  in
    runReader (partitions args) env

partitionsTests :: [Test]
partitionsTests =
  [ "finds empty partition for 0" ~:
    fromList [[]] @=? partitions'' 0
  , "finds no partitions for -1" ~:
    fromList [] @=? partitions'' (-1)
  , "finds partition for 2" ~:
    fromList [[1, 1], [2]] @=? partitions'' 2
  , "finds partitions for 7" ~:
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
    in
      fromList partitionsOf7 @=? partitions'' 7
  ]

appendOneAndMaybeIncrementLastTests :: [Test]
appendOneAndMaybeIncrementLastTests =
  [ "appends 1 when given an empty partition" ~:
    fromList [[1]] @=? appendOneAndMaybeIncrementLast []
  , "appends 1 and increments last given a partition of size 1" ~:
    fromList [[3, 1], [4]] @=? appendOneAndMaybeIncrementLast [3]
  , "appends 1 given a partition with 2 equal numbers" ~:
    fromList [[3, 3, 1]] @=? appendOneAndMaybeIncrementLast [3, 3]
  , "appends 1 and increments last given a partition with " ++
    "2 numbers and the second number is less than the first" ~:
    fromList [[5, 3, 1], [5, 4]] @=? appendOneAndMaybeIncrementLast [5, 3]
  , "appends 1 given a partition where last 2 numbers are equal" ~:
    fromList [[3, 3, 3, 1]] @=? appendOneAndMaybeIncrementLast [3, 3, 3]
  , "appends 1 and increments last given a partition where the " ++
    "last number is less than the next to last number" ~:
    fromList [[7, 5, 3, 1], [7, 5, 4]] @=?
      appendOneAndMaybeIncrementLast [7, 5, 3]
  ]

tests :: [Test]
tests = partitionsTests
     ++ appendOneAndMaybeIncrementLastTests

