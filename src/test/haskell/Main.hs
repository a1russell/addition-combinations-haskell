import qualified Test.HUnit

import qualified PartitionsTest (tests)

main :: IO Test.HUnit.Counts
main =
  let
    allTests = [ PartitionsTest.tests
               ]
    test = Test.HUnit.test . concat $ allTests
  in do
    Test.HUnit.runTestTT test
