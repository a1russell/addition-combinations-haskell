module Partitions where

import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

type Partition = [Int]
type Partitions = Set.Set Partition
type PartitionList = [Partition]

partitions :: Int -> Partitions
partitions 0 = Set.empty
partitions x =
  let
    go allPartitions@(headPartition:_)
      | all (==1) headPartition = allPartitions
      | otherwise = go $ newPartitions ++ allPartitions
      where
        newPartition = expandedTailPartition headPartition
        newPartitions = newPartition : collapsedTailPartitions newPartition
  in
    Set.fromList $ go [[x]]

expandedTailPartition :: Partition -> Partition
expandedTailPartition (x:xs) = (x - 1 : 1 : xs)

collapsedTailPartitions :: Partition -> PartitionList
collapsedTailPartitions =
  let
    go _ [] = []
    go splitIndex partition@(x:xs)
      | length partition < splitIndex = [partition]
      | otherwise = collapsed ++ partitionsForHead ++ partitionsForSplit
      where
        collapsed = Maybe.catMaybes [headCollapsed, splitCollapsed]
        headCollapsed = collapseInto 1 partition
        splitCollapsed = collapseInto splitIndex partition
        partitionsForHead = emptyOr collapsedTailPartitions headCollapsed
        partitionsForSplit = emptyOr partitionsForNextSplit splitCollapsed
        partitionsForNextSplit = go $ splitIndex + 1
        emptyOr = maybe []
  in
    go 2

collapseInto :: Int -> Partition -> Maybe Partition
collapseInto _ [] = Nothing
collapseInto splitIndex xs
  | splitIndex < 1 = Nothing
  | length rhs < numberToCollapse = Nothing
  | collapsedRhsHead > last lhs = Nothing
  | otherwise = Just $ lhs ++ collapsedRhsHead : newRhsTail
  where
    splitXs = splitAt splitIndex xs
    lhs = fst splitXs
    rhs = snd splitXs
    numberToCollapse = 2
    splitRhs = splitAt numberToCollapse rhs
    collapsedRhsHead = sum $ fst splitRhs
    newRhsTail = snd splitRhs
