module Partitions.Internal where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

type Partition = [Int]
type Partitions = Set.Set Partition
type PartitionList = [Partition]

partitions :: Int -> Partitions
partitions x
  | x < 0 = Set.empty
  | x == 0 = Set.singleton []
  | otherwise = Set.fromList $ go [[x]]
  where
    go allPartitions
      | all (==1) headPartition = allPartitions
      | otherwise = go $ newPartitions ++ allPartitions
      where
        headPartition = head allPartitions
        newPartition = expandedTailPartition headPartition
        newPartitions = newPartition : collapsedTailPartitions newPartition

expandedTailPartition :: Partition -> Partition
expandedTailPartition allXs =
  let
    x = head allXs
    xs = tail allXs
  in
    x - 1 : 1 : xs

collapsedTailPartitions :: Partition -> PartitionList
collapsedTailPartitions =
  let
    go _ [] = []
    go splitIndex partition@(_:_)
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
