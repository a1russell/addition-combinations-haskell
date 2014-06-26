module PartitionsTest
  ( tests
  ) where

import Test.HUnit

import Partitions

partitionsCountTests :: [Test]
partitionsCountTests =
  [ "count of partitions of -1 is 0" ~:
    0 @=? partitionsCount (-1)
  , "count of partitions of 0 is 1" ~:
    1 @=? partitionsCount 0
  , "count of partitions of 1 is 1" ~:
    1 @=? partitionsCount 1
  , "count of partitions of 10 is 42" ~:
    42 @=? partitionsCount 10
  ]

tests :: [Test]
tests = partitionsCountTests
