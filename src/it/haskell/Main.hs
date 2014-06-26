import Control.Monad (liftM)
import qualified Data.Set as Set
import qualified System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import Partitions.Internal

prop_partitionSumIsArgument :: Int -> Bool
prop_partitionSumIsArgument x =
  all (\y -> sum y == x) (Set.toList . partitions $ x)

main :: IO ()
main =
  let
    isSuccessM = liftM isSuccess
    checkTwentyTimes = quickCheckWithResult stdArgs { maxSuccess = 20 }
    checkLimitedRange = checkTwentyTimes . forAll (choose (-4, 42))
  in do
    testsPassed <- isSuccessM . checkLimitedRange $ prop_partitionSumIsArgument
    if testsPassed
      then System.Exit.exitSuccess
      else System.Exit.exitFailure
