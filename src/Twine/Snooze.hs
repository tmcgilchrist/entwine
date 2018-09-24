{-# LANGUAGE NoImplicitPrelude #-}
module Twine.Snooze (
    snooze
  , module Twine.Data.Duration
  ) where

import           Control.Concurrent

import           Twine.P
import           Twine.Data.Duration

import           System.IO (IO)

snooze :: Duration -> IO ()
snooze =
  threadDelay . toMicroseconds
