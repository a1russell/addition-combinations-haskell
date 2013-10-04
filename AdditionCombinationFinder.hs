module AdditionCombinationFinder where

import qualified Data.Set as Set
import Data.Maybe
import Control.Monad

findAdditionCombinations :: Int -> Set.Set [Int]
findAdditionCombinations 0 = Set.empty
findAdditionCombinations x = Set.fromList $ findAdditionCombinations' [[x]]

findAdditionCombinations' :: [[Int]] -> [[Int]]
findAdditionCombinations' allXs@(x:_)
    | all (==1) x = allXs
    | otherwise = findAdditionCombinations' $
                  (findAdditionCombinations'' x) ++ allXs

findAdditionCombinations'' :: [Int] -> [[Int]]
findAdditionCombinations'' x =
    let newCombination = moveOneFromHeadToTail x
    in catMaybes $ (Just newCombination) : (findTails (Just newCombination))

moveOneFromHeadToTail :: [Int] -> [Int]
moveOneFromHeadToTail (x:xs) = (x - 1 : 1 : xs)

findTails :: Maybe [Int] -> [Maybe [Int]]
findTails Nothing = [Nothing]
findTails (Just []) = [Nothing]
findTails (Just allXs@(_:[])) = [Just allXs]
findTails (Just allXs@(x:x':xs)) =
    let xTail = x' : xs
        first2x = [x, x']
        absorbed = absorbTail (Just [x]) (Just xTail)
        absorbed' = absorbTail (Just first2x) (Just xs)
    in [absorbed, absorbed'] ++ (findTails absorbed) ++
       (findTails absorbed')

absorbTail :: Maybe [Int] -> Maybe [Int] -> Maybe [Int]
absorbTail Nothing _ = Nothing
absorbTail _ Nothing = Nothing
absorbTail (Just []) _ = Nothing
absorbTail _ (Just []) = Nothing
absorbTail _ (Just (_:[])) = Nothing
absorbTail (Just lhs) (Just rhs@(x:x':xs))
    | xSum > (last lhs) = Nothing
    | otherwise = Just (lhs ++ (xSum : xs))
    where xSum = x + x'
