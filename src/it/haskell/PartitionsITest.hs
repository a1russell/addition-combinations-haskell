module PartitionsITest where

import Data.Set (toList)
import Test.QuickCheck

import Partitions

prop_partitionSumIsArgument :: Int -> Bool
prop_partitionSumIsArgument x =
  all (\y -> sum y == x) (toList . partitions $ x)

test :: IO Result
test =
  let
    checkTwentyTimes = quickCheckWithResult stdArgs { maxSuccess = 20 }
    checkLimitedRange = checkTwentyTimes . forAll (choose (-4, 42))
  in
    checkLimitedRange prop_partitionSumIsArgument

