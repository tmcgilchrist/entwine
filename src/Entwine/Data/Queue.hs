{-# LANGUAGE NoImplicitPrelude #-}
module Entwine.Data.Queue (
    Queue
  , newQueue
  , readQueue
  , tryReadQueue
  , writeQueue
  , isQueueEmpty
  ) where

import           Control.Concurrent.STM.TBQueue ( TBQueue, newTBQueue, tryReadTBQueue
                                                , readTBQueue, writeTBQueue, isEmptyTBQueue)

import           GHC.Conc (atomically)

import           Entwine.P

import           System.IO (IO)

-- |
--
-- A queue is an abstract type, representing a simple bounded FIFO channel
-- that can only have its state queried in a non-blocking manner.
--
-- This useful for implementing things like passing data between a
-- producer and consumer process with back presure.
--
newtype Queue a =
  Queue {
      queue :: TBQueue a
    }

newQueue :: Int -> IO (Queue a)
newQueue i =
  atomically $ Queue <$> newTBQueue i

readQueue :: Queue a -> IO a
readQueue =
  atomically . readTBQueue . queue

tryReadQueue :: Queue a -> IO (Maybe a)
tryReadQueue =
  atomically . tryReadTBQueue . queue

writeQueue :: Queue a -> a -> IO ()
writeQueue q =
  atomically . writeTBQueue (queue q)

isQueueEmpty :: Queue a -> IO Bool
isQueueEmpty =
  atomically . isEmptyTBQueue . queue
