module PartitionsITest where

import qualified Data.IntMultiSet as IntMultiSet
import Data.Set (toList)
import Test.QuickCheck

import Partitions

prop_partitionSumIsArgument :: Int -> Bool
prop_partitionSumIsArgument x =
  all (\y -> sum y == x) (map IntMultiSet.toList (toList . partitions $ x))

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
      , checkLimitedRange prop_numberOfPartitions
      ]

