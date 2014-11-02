module Partitions.Internal where

import Control.Monad (liftM2)
import Control.Monad.Reader (Reader, asks)
import Data.Foldable (foldMap)
import Data.Set
  ( Set
  , empty
  , insert
  , singleton
  )

type Partition = [Int]
type Partitions = Set Partition

data PartitionsEnv = PartitionsEnv
  { insertOneAndMaybeIncrementLeast' :: Partition -> Partitions
  }

type PartitionsReader = Reader PartitionsEnv

partitions :: Int -> PartitionsReader Partitions
partitions x
  | x < 0 = return empty
  | x == 0 = return $ singleton []
  | otherwise =
      liftM2 foldMap
        (asks insertOneAndMaybeIncrementLeast')
        (partitions (x - 1))

insertOneAndMaybeIncrementLeast :: Partition -> Partitions
insertOneAndMaybeIncrementLeast partition =
  let
    hasOnlyOneOfLeast =
      not (null partition) &&
        ((length partition < 2) ||
         (head (tail partition) > head partition))
    maybePartitionWithIncrementedLast =
      if hasOnlyOneOfLeast
        then singleton (head partition + 1 : tail partition)
        else empty
  in
    insert (1 : partition) maybePartitionWithIncrementedLast

