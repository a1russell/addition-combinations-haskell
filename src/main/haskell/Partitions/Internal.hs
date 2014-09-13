module Partitions.Internal where

import qualified Control.Monad as Monad
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
  | otherwise = Monad.liftM Set.fromList $ go [[x]]
  where
    expandedTailPartition'' = Reader.asks . flip expandedTailPartition'
    collapsedTailPartitions'' = Reader.asks . flip collapsedTailPartitions'
    go allPartitions
      | all (==1) headPartition = return allPartitions
      | otherwise = go . (++ allPartitions) =<< newPartitions
      where
        headPartition = head allPartitions
        newPartition = expandedTailPartition'' headPartition
        newCollapsedTailPartitions = collapsedTailPartitions'' =<< newPartition
        (<:>) = Monad.liftM2 (:)
        newPartitions = newPartition <:> newCollapsedTailPartitions

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
    collapseInto'' splitIndex xs =
      Reader.asks $ \env -> collapseInto' env splitIndex xs
    go _ [] = return []
    go splitIndex partition@(_:_)
      | length partition < splitIndex = return [partition]
      | otherwise = collapsed <++> partitionsForHead <++> partitionsForSplit
      where
        (<++>) = Monad.liftM2 (++)
        collapsed = Monad.liftM Maybe.catMaybes $
                    Monad.sequence [headCollapsed, splitCollapsed]
        headCollapsed = collapseInto'' 1 partition
        splitCollapsed = collapseInto'' splitIndex partition
        partitionsForHead = emptyOr collapsedTailPartitions =<< headCollapsed
        partitionsForSplit = emptyOr partitionsForNextSplit =<< splitCollapsed
        partitionsForNextSplit = go $ splitIndex + 1
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

