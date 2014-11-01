module PartitionsITest where

import Data.Set (toList)
import Test.QuickCheck

import Partitions

prop_partitionSumIsArgument :: Int -> Bool
prop_partitionSumIsArgument x =
  all (\y -> sum y == x) (toList . partitions $ x)

prop_partitionIsOrdered :: Int -> Bool
prop_partitionIsOrdered x =
  let
    isDescending xs = and $ zipWith (>=) xs (tail xs)
  in
    all isDescending (toList . partitions $ x)

prop_numberOfPartitions :: Int -> Bool
prop_numberOfPartitions x =
  partitionsCount x == length (toList $ partitions x)

tests :: IO [Result]
tests =
  let
    checkTwentyTimes = quickCheckWithResult stdArgs { maxSuccess = 20 }
    checkLimitedRange = checkTwentyTimes . forAll (choose (-4, 42))
  in
    sequence
      [ checkLimitedRange prop_partitionSumIsArgument
      , checkLimitedRange prop_partitionIsOrdered
      , checkLimitedRange prop_numberOfPartitions
      ]

