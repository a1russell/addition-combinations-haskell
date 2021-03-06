module Partitions
  ( Intern.Partitions
  , partitions
  , partitionsCount
  ) where

import Control.Monad.Reader (runReader)

import qualified Partitions.Internal as Intern

partitions :: Int -> Intern.Partitions
partitions args =
  let
    partitionsEnv = Intern.PartitionsEnv
      Intern.insertOneAndMaybeIncrementLeast
  in
    runReader (Intern.partitions args) partitionsEnv

partitionsCount :: Int -> Int
partitionsCount =
  let
    partitionsCount' m n
      | n < 0  = 0
      | n == 0 = 1
      | otherwise = sum [partitionsCount' i (n - i) | i <- [m..n]]
  in
    partitionsCount' 1
