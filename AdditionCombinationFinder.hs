module AdditionCombinationFinder where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

additionCombinations :: Int -> Set.Set [Int]
additionCombinations 0 = Set.empty
additionCombinations x = Set.fromList $ additionCombinations' [[x]]

additionCombinations' :: [[Int]] -> [[Int]]
additionCombinations' allCombos@(headCombo:_)
    | all (==1) headCombo = allCombos
    | otherwise = additionCombinations' $ newCombos ++ allCombos
    where newCombo = Just . expandedTailCombination $ headCombo
          newCombos = Maybe.catMaybes $ newCombo : collapsedTailCombos newCombo

expandedTailCombination :: [Int] -> [Int]
expandedTailCombination (x:xs) = (x - 1 : 1 : xs)

collapsedTailCombos :: Maybe [Int] -> [Maybe [Int]]
collapsedTailCombos = collapsedTailCombos' 2

collapsedTailCombos' :: Int -> Maybe [Int] -> [Maybe [Int]]
collapsedTailCombos' _ Nothing = [Nothing]
collapsedTailCombos' _ (Just []) = [Nothing]
collapsedTailCombos' splitIndex (Just allXs@(x:xs))
    | length allXs < splitIndex = [Just allXs]
    | otherwise =
        [collapsed, collapsed'] ++
        collapsedTailCombos collapsed ++
        collapsedTailCombos' (splitIndex + 1) collapsed'
    where splitXs = splitAt splitIndex allXs
          firstXs = fst splitXs
          lastXs = snd splitXs
          collapsed = collapseInto 1 allXs
          collapsed' = collapseInto splitIndex allXs

collapseInto :: Int -> [Int] -> Maybe [Int]
collapseInto _ [] = Nothing
collapseInto splitIndex xs
    | splitIndex < 1 = Nothing
    | length rhs < numberToCollapse = Nothing
    | collapsedRhsHead > last lhs = Nothing
    | otherwise = Just $ lhs ++ collapsedRhsHead : newRhsTail
    where splitXs = splitAt splitIndex xs
          lhs = fst splitXs
          rhs = snd splitXs
          numberToCollapse = 2
          splitRhs = splitAt numberToCollapse rhs
          collapsedRhsHead = sum $ fst splitRhs
          newRhsTail = snd splitRhs
