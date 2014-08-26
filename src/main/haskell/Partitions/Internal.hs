module Partitions.Internal where

import qualified Control.Monad.Reader as Reader
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

type Partition = [Int]
type Partitions = Set.Set Partition
type PartitionList = [Partition]

data PartitionsEnv = PartitionsEnv
  { expandedTailPartition' :: Partition -> Partition
  , collapsedTailPartitions' :: Partition -> PartitionList
  }

type PartitionsReader = Reader.Reader PartitionsEnv

partitions :: Int -> PartitionsReader Partitions
partitions x
  | x < 0 = return Set.empty
  | x == 0 = return $ Set.singleton []
  | otherwise = Reader.liftM Set.fromList $ go [[x]]
  where
    go :: PartitionList -> PartitionsReader PartitionList
    go allPartitions
      | all (==1) headPartition = return allPartitions
      | otherwise = do
          env <- Reader.ask
          let newPartitions' = Reader.runReader newPartitions env
          go $ newPartitions' ++ allPartitions
      where
        headPartition :: Partition
        headPartition = head allPartitions
        newPartition :: PartitionsReader Partition
        newPartition = do
           expandedTailPartition'' <- Reader.asks expandedTailPartition'
           return $ expandedTailPartition'' headPartition
        newPartitions :: PartitionsReader PartitionList
        newPartitions = do
           env <- Reader.ask
           let collapsedTailPartitions'' = collapsedTailPartitions' env
           let newPartition' = Reader.runReader newPartition env
           return $ newPartition' : collapsedTailPartitions'' newPartition'

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
