import qualified Test.HUnit

import qualified PartitionsTest as AC (tests)

main :: IO Test.HUnit.Counts
main =
  let
    allTests = [ AC.tests
               ]
    test = Test.HUnit.test . concat $ allTests
  in do
    Test.HUnit.runTestTT test
