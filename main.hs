import qualified Data.Set as Set

import AdditionCombinationFinder

main :: IO ()
main = mapM_ print $ Set.toList . additionCombinations $ 5
