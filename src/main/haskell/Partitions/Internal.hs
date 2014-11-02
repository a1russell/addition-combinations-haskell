module Partitions.Internal where

import Control.Monad (liftM2)
import Control.Monad.Reader (Reader, asks)
import Data.Foldable (foldMap)
import Data.IntMultiSet (IntMultiSet)
import qualified Data.IntMultiSet as IntMultiSet
import Data.Set
  ( Set
  , empty
  , insert
  , singleton
  )

type Partition = IntMultiSet
type Partitions = Set Partition

data PartitionsEnv = PartitionsEnv
  { insertOneAndMaybeIncrementLeast' :: Partition -> Partitions
  }

type PartitionsReader = Reader PartitionsEnv

partitions :: Int -> PartitionsReader Partitions
partitions x
  | x < 0 = return empty
  | x == 0 = return $ singleton IntMultiSet.empty
  | otherwise =
      liftM2 foldMap
        (asks insertOneAndMaybeIncrementLeast')
        (partitions (x - 1))

insertOneAndMaybeIncrementLeast :: Partition -> Partitions
insertOneAndMaybeIncrementLeast partition =
  let
    leastInPartition = IntMultiSet.findMin partition
    hasOnlyOneOfLeast =
      not (IntMultiSet.null partition) &&
        ((IntMultiSet.size partition < 2) ||
         (IntMultiSet.occur leastInPartition partition == 1))
    maybePartitionWithIncrementedLast =
      if hasOnlyOneOfLeast
        then singleton
          (IntMultiSet.insert
            (leastInPartition + 1)
            (IntMultiSet.deleteMin partition))
        else empty
  in
    insert
      (IntMultiSet.insert 1 partition)
      maybePartitionWithIncrementedLast

