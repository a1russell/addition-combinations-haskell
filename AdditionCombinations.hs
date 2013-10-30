module AdditionCombinations where

import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

additionCombinations :: Int -> Set.Set [Int]
additionCombinations 0 = Set.empty
additionCombinations x = Set.fromList $ additionCombinations' [[x]]

additionCombinations' :: [[Int]] -> [[Int]]
additionCombinations' allCombos@(headCombo:_)
    | all (==1) headCombo = allCombos
    | otherwise = additionCombinations' $ newCombos ++ allCombos
    where newCombo = expandedTailCombination headCombo
          newCombos = newCombo : collapsedTailCombos newCombo

expandedTailCombination :: [Int] -> [Int]
expandedTailCombination (x:xs) = (x - 1 : 1 : xs)

collapsedTailCombos :: [Int] -> [[Int]]
collapsedTailCombos = collapsedTailCombos' 2

collapsedTailCombos' :: Int -> [Int] -> [[Int]]
collapsedTailCombos' _ [] = []
collapsedTailCombos' splitIndex allXs@(x:xs)
    | length allXs < splitIndex = [allXs]
    | otherwise = collapsed ++ collapsedIntoHead' ++ collapsedIntoSplit'
    where collapsed = Maybe.catMaybes [collapsedIntoHead, collapsedIntoSplit]
          collapsedIntoHead = collapseInto 1 allXs
          collapsedIntoSplit = collapseInto splitIndex allXs
          collapsedIntoHead' = emptyOr collapsedTailCombos collapsedIntoHead
          collapsedIntoSplit' = emptyOr combosForNextSplit collapsedIntoSplit
          combosForNextSplit = collapsedTailCombos' $ splitIndex + 1
          emptyOr = maybe []

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
