import qualified Data.Set as Set

import AdditionCombinations

main :: IO ()
main = mapM_ print $ Set.toList . additionCombinations $ 5
