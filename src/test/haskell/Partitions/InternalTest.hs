module Partitions.InternalTest
  ( tests
  ) where

import Control.Monad.Reader (runReader)
import qualified Data.IntMultiSet as IntMultiSet
import Data.Set (fromList, singleton)

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
    singleton IntMultiSet.empty @=? partitions'' 0
  , "finds no partitions for -1" ~:
    fromList [] @=? partitions'' (-1)
  , "finds partition for 2" ~:
    let
      partitionsOf2 = fromList
        [ IntMultiSet.fromAscList [1, 1]
        , IntMultiSet.singleton 2
        ]
    in
      partitionsOf2 @=? partitions'' 2
  , "finds partitions for 7" ~:
    let
      partitionsOf7 = fromList
        [ IntMultiSet.fromAscList [1, 1, 1, 1, 1, 1, 1]
        , IntMultiSet.fromAscList [1, 2, 2, 2]
        , IntMultiSet.fromAscList [1, 1, 1, 2, 2]
        , IntMultiSet.fromAscList [1, 1, 1, 1, 1, 2]
        , IntMultiSet.fromAscList [2, 2, 3]
        , IntMultiSet.fromAscList [1, 3, 3]
        , IntMultiSet.fromAscList [1, 1, 2, 3]
        , IntMultiSet.fromAscList [1, 1, 1, 1, 3]
        , IntMultiSet.fromAscList [3, 4]
        , IntMultiSet.fromAscList [1, 2, 4]
        , IntMultiSet.fromAscList [1, 1, 1, 4]
        , IntMultiSet.fromAscList [2, 5]
        , IntMultiSet.fromAscList [1, 1, 5]
        , IntMultiSet.fromAscList [1, 6]
        , IntMultiSet.singleton    7
        ]
    in
      partitionsOf7 @=? partitions'' 7
  ]

insertOneAndMaybeIncrementLeastTests :: [Test]
insertOneAndMaybeIncrementLeastTests =
  [ "inserts 1 when given an empty partition" ~:
    singleton (IntMultiSet.singleton 1) @=?
      insertOneAndMaybeIncrementLeast IntMultiSet.empty
  , "inserts 1 and increments least given a partition of size 1" ~:
    let
      expected = fromList
        [ IntMultiSet.fromAscList [1, 3]
        , IntMultiSet.singleton    4
        ]
      partition = IntMultiSet.singleton 3
    in
      expected @=? insertOneAndMaybeIncrementLeast partition
  , "inserts 1 given a partition with 2 equal numbers" ~:
    let
      expected = singleton $ IntMultiSet.fromAscList [1, 3, 3]
      partition = IntMultiSet.fromAscList [3, 3]
    in
      expected @=? insertOneAndMaybeIncrementLeast partition
  , "inserts 1 and increments least given partition with 2 unequal numbers" ~:
    let
      expected = fromList
        [ IntMultiSet.fromAscList [1, 3, 5]
        , IntMultiSet.fromAscList [4, 5]
        ]
      partition = IntMultiSet.fromAscList [3, 5]
    in
      expected @=? insertOneAndMaybeIncrementLeast partition
  , "inserts 1 given partition with multiple instances of the least number" ~:
    let
      expected = singleton $ IntMultiSet.fromAscList [1, 3, 3, 3]
      partition = IntMultiSet.fromAscList [3, 3, 3]
    in
      expected @=? insertOneAndMaybeIncrementLeast partition
  , "inserts 1 and increments least given a partition with only one " ++
    "instance of the least number" ~:
    let
      expected = fromList
        [ IntMultiSet.fromAscList [1, 3, 5, 7]
        , IntMultiSet.fromAscList [4, 5, 7]
        ]
      partition = IntMultiSet.fromAscList [3, 5, 7]
    in
      expected @=? insertOneAndMaybeIncrementLeast partition
  ]

tests :: [Test]
tests = partitionsTests
     ++ insertOneAndMaybeIncrementLeastTests

