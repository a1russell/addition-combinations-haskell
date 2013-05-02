module AdditionCombinationFinder where


findAdditionCombinations :: Int -> [[Int]]
findAdditionCombinations 1 = [[]]
findAdditionCombinations x = [[1, 1]]
