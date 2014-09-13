module Partitions.Internal where

import Control.Monad (liftM, liftM2)
import Control.Monad.Reader (Reader, asks)
import Data.Maybe (catMaybes)
import Data.Set
  ( Set
  , empty
  , fromList
  , singleton
  )

type Partition = [Int]
type Partitions = Set Partition
type PartitionList = [Partition]

data PartitionsEnv = PartitionsEnv
  { expandedTailPartition' :: Partition -> Partition
  , collapsedTailPartitions' :: Partition -> PartitionList
  }

type PartitionsReader = Reader PartitionsEnv

partitions :: Int -> PartitionsReader Partitions
partitions x
  | x < 0 = return empty
  | x == 0 = return $ singleton []
  | otherwise = liftM fromList $ go [[x]]
  where
    expandedTailPartition'' = asks . flip expandedTailPartition'
    collapsedTailPartitions'' = asks . flip collapsedTailPartitions'
    go allPartitions
      | all (==1) headPartition = return allPartitions
      | otherwise = go . (++ allPartitions) =<< newPartitions
      where
        headPartition = head allPartitions
        newPartition = expandedTailPartition'' headPartition
        newCollapsedTailPartitions = collapsedTailPartitions'' =<< newPartition
        (<:>) = liftM2 (:)
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

type CollapsedTailPartsReader = Reader CollapsedTailPartitionsEnv

collapsedTailPartitions :: Partition -> CollapsedTailPartsReader PartitionList
collapsedTailPartitions =
  let
    collapseInto'' splitIndex xs =
      asks $ \env -> collapseInto' env splitIndex xs
    go _ [] = return []
    go splitIndex partition@(_:_)
      | length partition < splitIndex = return [partition]
      | otherwise = collapsed <++> partitionsForHead <++> partitionsForSplit
      where
        (<++>) = liftM2 (++)
        collapsed = liftM catMaybes $
                    sequence [headCollapsed, splitCollapsed]
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

