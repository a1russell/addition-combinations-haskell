module AdditionCombinations where

import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

type Combo = [Int]
type Combos = [Combo]

additionCombinations :: Int -> Set.Set Combo
additionCombinations 0 = Set.empty
additionCombinations x = Set.fromList $ additionCombinations' [[x]]

additionCombinations' :: Combos -> Combos
additionCombinations' allCombos@(headCombo:_)
    | all (==1) headCombo = allCombos
    | otherwise = additionCombinations' $ newCombos ++ allCombos
    where newCombo = expandedTailCombination headCombo
          newCombos = newCombo : collapsedTailCombos newCombo

expandedTailCombination :: Combo -> Combo
expandedTailCombination (x:xs) = (x - 1 : 1 : xs)

collapsedTailCombos :: Combo -> Combos
collapsedTailCombos = collapsedTailCombos' 2

collapsedTailCombos' :: Int -> Combo -> Combos
collapsedTailCombos' _ [] = []
collapsedTailCombos' splitIndex combo@(x:xs)
    | length combo < splitIndex = [combo]
    | otherwise = collapsed ++ collapsedIntoHead' ++ collapsedIntoSplit'
    where collapsed = Maybe.catMaybes [collapsedIntoHead, collapsedIntoSplit]
          collapsedIntoHead = collapseInto 1 combo
          collapsedIntoSplit = collapseInto splitIndex combo
          collapsedIntoHead' = emptyOr collapsedTailCombos collapsedIntoHead
          collapsedIntoSplit' = emptyOr combosForNextSplit collapsedIntoSplit
          combosForNextSplit = collapsedTailCombos' $ splitIndex + 1
          emptyOr = maybe []

collapseInto :: Int -> Combo -> Maybe Combo
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
