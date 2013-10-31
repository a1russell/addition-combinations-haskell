module AdditionCombinations where

import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

type Combo = [Int]
type Combos = [Combo]

additionCombinations :: Int -> Set.Set Combo
additionCombinations 0 = Set.empty
additionCombinations x = let
    go allCombos@(headCombo:_)
        | all (==1) headCombo = allCombos
        | otherwise = go $ newCombos ++ allCombos
        where newCombo = expandedTailCombination headCombo
              newCombos = newCombo : collapsedTailCombos newCombo
    in Set.fromList $ go [[x]]

expandedTailCombination :: Combo -> Combo
expandedTailCombination (x:xs) = (x - 1 : 1 : xs)

collapsedTailCombos :: Combo -> Combos
collapsedTailCombos = let
    go _ [] = []
    go splitIndex combo@(x:xs)
        | length combo < splitIndex = [combo]
        | otherwise = collapsed ++ combosForHead ++ combosForSplit
        where collapsed = Maybe.catMaybes [headCollapsed, splitCollapsed]
              headCollapsed = collapseInto 1 combo
              splitCollapsed = collapseInto splitIndex combo
              combosForHead = emptyOr collapsedTailCombos headCollapsed
              combosForSplit = emptyOr combosForNextSplit splitCollapsed
              combosForNextSplit = go $ splitIndex + 1
              emptyOr = maybe []
    in go 2

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
