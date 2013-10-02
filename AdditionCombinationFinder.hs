module AdditionCombinationFinder where


findAdditionCombinations :: Int -> [[Int]]
findAdditionCombinations 0 = []
findAdditionCombinations x = findAdditionCombinations' [x]

findAdditionCombinations' :: [Int] -> [[Int]]
findAdditionCombinations' (x:xs) = (x - 1 : 1 : xs) : [x : xs]
