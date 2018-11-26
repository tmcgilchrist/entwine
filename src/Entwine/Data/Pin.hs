{-# LANGUAGE NoImplicitPrelude #-}
module Entwine.Data.Pin (
  -- * Types
    Pin
  -- * Functions
  , newPin
  , checkPin
  , waitForPin
  , pullPin
  ) where

import           Control.Concurrent.MVar (MVar, newEmptyMVar, tryTakeMVar
                                         , readMVar, tryPutMVar)

import           Entwine.P

import           System.IO (IO)

-- |
-- A pin is an abstract type, representing a simple barrier
-- that can only have its state queried in a non-blocking
-- manner.
--
-- This useful for implementing things like passing in a hook
-- to terminate or stop waiting for a process.
--
newtype Pin =
  Pin { pin :: MVar () }

newPin :: IO Pin
newPin =
  Pin <$> newEmptyMVar

checkPin :: Pin -> IO Bool
checkPin =
  fmap isJust . tryTakeMVar . pin

waitForPin :: Pin -> IO ()
waitForPin =
  void . readMVar . pin

pullPin :: Pin -> IO ()
pullPin =
  void . flip tryPutMVar () . pin
