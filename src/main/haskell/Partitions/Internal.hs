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

newtype CollapsedTailPartitionsEnv = CollapsedTailPartitionsEnv
  { collapseInto' :: Int -> Partition -> Maybe Partition }

type CollapsedTailPartsReader = Reader.Reader CollapsedTailPartitionsEnv

collapsedTailPartitions :: Partition -> CollapsedTailPartsReader PartitionList
collapsedTailPartitions =
  let
    go :: Int -> Partition -> CollapsedTailPartsReader PartitionList
    go _ [] = return []
    go splitIndex partition@(_:_)
      | length partition < splitIndex = return [partition]
      | otherwise = collapsed ++^ partitionsForHead ++^ partitionsForSplit
      where
        (++^) :: CollapsedTailPartsReader PartitionList
              -> CollapsedTailPartsReader PartitionList
              -> CollapsedTailPartsReader PartitionList
        (++^) = Reader.liftM2 (++)
        collapsed :: CollapsedTailPartsReader PartitionList
        collapsed = Reader.liftM Maybe.catMaybes $
                    Reader.sequence [headCollapsed, splitCollapsed]
        collapseInto'' :: Int
                       -> Partition
                       -> CollapsedTailPartsReader (Maybe Partition)
        collapseInto'' splitIndex' xs = do
          collapseInto''' <- Reader.asks collapseInto'
          return $ collapseInto''' splitIndex' xs
        headCollapsed :: CollapsedTailPartsReader (Maybe Partition)
        headCollapsed = collapseInto'' 1 partition
        splitCollapsed :: CollapsedTailPartsReader (Maybe Partition)
        splitCollapsed = collapseInto'' splitIndex partition
        partitionsForHead :: CollapsedTailPartsReader PartitionList
        partitionsForHead = do
          env <- Reader.ask
          let headCollapsed' = Reader.runReader headCollapsed env
          emptyOr collapsedTailPartitions headCollapsed'
        partitionsForSplit :: CollapsedTailPartsReader PartitionList
        partitionsForSplit = do
          env <- Reader.ask
          let splitCollapsed' = Reader.runReader splitCollapsed env
          emptyOr partitionsForNextSplit splitCollapsed'
        partitionsForNextSplit :: Partition
                               -> CollapsedTailPartsReader PartitionList
        partitionsForNextSplit = go $ splitIndex + 1
        emptyOr :: (Partition -> CollapsedTailPartsReader PartitionList)
                -> Maybe Partition
                -> CollapsedTailPartsReader PartitionList
        emptyOr = maybe $ return []
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
