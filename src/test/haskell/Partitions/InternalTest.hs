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
    env = PartitionsEnv insertOneAndMaybeIncrementLeast
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
        , [1, 2, 2, 2]
        , [1, 1, 1, 2, 2]
        , [1, 1, 1, 1, 1, 2]
        , [2, 2, 3]
        , [1, 3, 3]
        , [1, 1, 2, 3]
        , [1, 1, 1, 1, 3]
        , [3, 4]
        , [1, 2, 4]
        , [1, 1, 1, 4]
        , [2, 5]
        , [1, 1, 5]
        , [1, 6]
        , [7]
        ]
    in
      fromList partitionsOf7 @=? partitions'' 7
  ]

insertOneAndMaybeIncrementLeastTests :: [Test]
insertOneAndMaybeIncrementLeastTests =
  [ "inserts 1 when given an empty partition" ~:
    fromList [[1]] @=? insertOneAndMaybeIncrementLeast []
  , "inserts 1 and increments least given a partition of size 1" ~:
    fromList [[1, 3], [4]] @=? insertOneAndMaybeIncrementLeast [3]
  , "inserts 1 given a partition with 2 equal numbers" ~:
    fromList [[1, 3, 3]] @=? insertOneAndMaybeIncrementLeast [3, 3]
  , "inserts 1 and increments least given partition with 2 unequal numbers" ~:
    fromList [[1, 3, 5], [4, 5]] @=? insertOneAndMaybeIncrementLeast [3, 5]
  , "inserts 1 given partition with multiple instances of the least number" ~:
    fromList [[1, 3, 3, 3]] @=? insertOneAndMaybeIncrementLeast [3, 3, 3]
  , "inserts 1 and increments least given a partition with only one " ++
    "instance of the least number" ~:
    fromList [[1, 3, 5, 7], [4, 5, 7]] @=?
      insertOneAndMaybeIncrementLeast [3, 5, 7]
  ]

tests :: [Test]
tests = partitionsTests
     ++ insertOneAndMaybeIncrementLeastTests

