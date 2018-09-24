{-# LANGUAGE NoImplicitPrelude #-}
module Entwine.Snooze (
    snooze
  , module Entwine.Data.Duration
  ) where

import           Control.Concurrent

import           Entwine.P
import           Entwine.Data.Duration

import           System.IO (IO)

snooze :: Duration -> IO ()
snooze =
  threadDelay . toMicroseconds
