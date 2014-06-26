import qualified System.Exit
import qualified Test.HUnit

import PartitionsTest (tests)
import Partitions.InternalTest (tests)

main :: IO ()
main =
  let
    allTests = [ PartitionsTest.tests
               , Partitions.InternalTest.tests
               ]
    test = Test.HUnit.test . concat $ allTests
  in do
    counts <- Test.HUnit.runTestTT test
    let failures = Test.HUnit.failures counts
    let errors = Test.HUnit.errors counts
    if failures == 0 && errors == 0
      then System.Exit.exitSuccess
      else System.Exit.exitFailure
