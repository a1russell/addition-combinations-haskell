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
  { appendOneAndMaybeIncrementLast' :: Partition -> Partitions
  }

type PartitionsReader = Reader PartitionsEnv

partitions :: Int -> PartitionsReader Partitions
partitions x
  | x < 0 = return empty
  | x == 0 = return $ singleton []
  | otherwise =
      liftM2 foldMap
        (asks appendOneAndMaybeIncrementLast')
        (partitions (x - 1))

appendOneAndMaybeIncrementLast :: Partition -> Partitions
appendOneAndMaybeIncrementLast partition =
  let
    lastLessThanNextToLast  =
      not (null partition) &&
        ((length partition < 2) ||
         (last (init partition) > last partition))
    maybePartitionWithIncrementedLast =
      if lastLessThanNextToLast
        then singleton (init partition ++ [last partition + 1])
        else empty
  in
    insert (partition ++ [1]) maybePartitionWithIncrementedLast

