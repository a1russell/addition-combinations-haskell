import Control.Monad (liftM)
import qualified System.Exit
import Test.QuickCheck.Test (isSuccess)

import PartitionsITest (tests)

main :: IO ()
main = do
  testsPassed <- liftM (all isSuccess) tests
  if testsPassed
    then System.Exit.exitSuccess
    else System.Exit.exitFailure

