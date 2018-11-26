{-# LANGUAGE NoImplicitPrelude #-}
module Entwine.Snooze (
    snooze
  , module X
  ) where

import           Control.Concurrent (threadDelay)

import           Entwine.P
import           Entwine.Data.Duration as X

import           System.IO (IO)

-- | Snooze for a short duration
--
snooze :: Duration -> IO ()
snooze =
  threadDelay . toMicroseconds
