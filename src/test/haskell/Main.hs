import qualified System.Exit
import qualified Test.HUnit

import qualified PartitionsTest (tests)

main :: IO Test.HUnit.Counts
main =
  let
    allTests = [ PartitionsTest.tests
               ]
    test = Test.HUnit.test . concat $ allTests
  in do
    counts <- Test.HUnit.runTestTT test
    let failures = Test.HUnit.failures counts
    let errors = Test.HUnit.errors counts
    if failures == 0 && errors == 0
      then System.Exit.exitSuccess
      else System.Exit.exitFailure
