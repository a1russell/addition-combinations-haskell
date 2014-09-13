import Control.Monad (liftM)
import qualified System.Exit
import Test.QuickCheck.Test (isSuccess)

import PartitionsITest (test)

main :: IO ()
main = do
  testsPassed <- liftM isSuccess test
  if testsPassed
    then System.Exit.exitSuccess
    else System.Exit.exitFailure

